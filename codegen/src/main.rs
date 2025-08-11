// Yes i know the error handling & readability is abysmal,
// but this only really has to run once, so it should be fine

#![feature(iter_intersperse)]
#![feature(exit_status_error)]
#![feature(string_remove_matches)]
#![feature(super_let)]
#![feature(iter_map_windows)]

use balatro::{JokerCompatibility, JokerEffectType};
use futures::future::join_all;
use isahc::AsyncReadResponseExt;
use parse_wiki_text::{Node, Parameter};
use percent_encoding::{NON_ALPHANUMERIC, utf8_percent_encode};
use serde::Deserialize;
use smol::fs;
use std::{env, fmt::Write, path::PathBuf, process::Command, sync::LazyLock};

static ROOT_DIR: LazyLock<PathBuf> = LazyLock::new(|| {
    let mut found = false;
    env::current_dir()
        .unwrap()
        .iter()
        .take_while(|part| {
            if *part == "balatro-ai" {
                found = true;
                return false;
            };
            found
        })
        .collect::<PathBuf>()
});

fn main() {
    smol::block_on(async {
        let mut code = String::new();

        for consumable in consumables() {
            code.push_str(&consumable.await);
        }
        code.push_str(&jokers().await);

        // write code
        let lib_path = ROOT_DIR.join("balatro/src/lib.rs");

        const CODEGEN_START: &str = "// CODEGEN START";
        const CODEGEN_END: &str = "// CODEGEN END";

        let mut lib_string = fs::read_to_string(&lib_path).await.unwrap();

        let start_codegen = lib_string.find(CODEGEN_START).unwrap() + CODEGEN_START.len();
        let end_codegen = lib_string.find(CODEGEN_END).unwrap() - 1;

        lib_string.replace_range(start_codegen..end_codegen, &code);

        fs::write(&lib_path, lib_string).await.unwrap();

        // format code
        Command::new("cargo")
            .args(["fmt", "-p", "balatro"])
            .spawn()
            .unwrap()
            .wait()
            .unwrap()
            .exit_ok()
            .unwrap();
    })
}

fn consumables() -> [impl Future<Output = String>; 3] {
    ["Tarot", "Planet", "Spectral"].map(consumable)
}

async fn consumable(name: &str) -> String {
    #[derive(Deserialize, Debug)]
    struct ExpandTemplates {
        expandtemplates: Wikitext,
    }
    #[derive(Deserialize, Debug)]
    struct Wikitext {
        wikitext: String,
    }
    let query: ExpandTemplates = isahc::get_async(format!("https://balatrowiki.org/api.php?action=expandtemplates&title=Tarot_Cards&text={}&prop=wikitext&format=json", utf8_percent_encode(&format!("{{{{Consumable Table|type=1|{name} Cards}}}}"), NON_ALPHANUMERIC))).await.unwrap().json().await.unwrap();
    let configuration = parse_wiki_text::Configuration::default();
    let parsed = configuration.parse(&query.expandtemplates.wikitext);

    // get table
    // TODO: The pattern is: Image, then relevant link
    let items = parsed
        .nodes
        .into_iter()
        .map_windows(|[node_a, node_b]| {
            if matches!(node_a, Node::Image { .. })
                && let Node::Link { target, .. } = node_b
                && !target.contains("Card Suits#")
            {
                Some(target.to_string())
            } else {
                None
            }
        })
        .flatten() // remove None's
        .collect::<Vec<_>>();

    // [buyprice, sellprice]
    let stats = join_all(items.iter().map(|item| async {
        #[derive(Deserialize)]
        struct ParseWikitext {
            parse: Wikitext,
        }

        let wikitext_string = isahc::get_async(format!(
            "https://balatrowiki.org/api.php?action=parse&page={}&prop=wikitext&format=json&formatversion=2",
            utf8_percent_encode(item, NON_ALPHANUMERIC)
        ))
        .await
        .unwrap()
        .json::<ParseWikitext>()
        .await
        .unwrap();

        let parsed = configuration.parse(&wikitext_string.parse.wikitext);

        let parameters = &parsed.nodes.iter().find_map(|node| if let Node::Template { name, parameters, .. } = node
            && let Node::Text{value: "Consumable info", ..} = name[0] {
                Some(parameters)
        } else {
            None
        }).unwrap();

        parameters.iter().map_windows(|parameters| {
            let [Parameter { name: name1,  value: value1, .. },
                Parameter { name: name2, value: value2, .. }] = parameters;

            if matches!(name1.as_deref()?[0], Node::Text { value: "buyprice", .. })
            {
                let extract_value = |value: &Vec<Node>| -> u8 {
                    let Node::Text { value, .. } =  value[0] else { panic!() };
                    value.split(' ').next().unwrap().parse::<u8>().unwrap()
                };
                let value1 = extract_value(value1);
                let value2 = if matches!(name2.as_deref()?[0], Node::Text { value: "sellprice", .. }) {
                    extract_value(value2)
                } else {
                    value1/2
                };
                Some([value1, value2])
            } else {
                None
            }
        }).flatten().next().unwrap()
    }))
    .await;

    // assets
    let assets = items
        .iter()
        .map(|item| {
            (
                smol::spawn(isahc::get_async(format!(
                    "https://balatrowiki.org/images/{}.png",
                    utf8_percent_encode(&item.replace(' ', "_"), NON_ALPHANUMERIC)
                ))),
                item.clone(),
            )
        })
        .collect();
    handle_assets(assets).await;

    consumables_codegen(name, items, stats).await
}

async fn consumables_codegen(name: &str, names: Vec<String>, stats: Vec<[u8; 2]>) -> String {
    let mut code = String::new();

    let variants = names
        .iter()
        .map(|item| sanitize_variant_name(item))
        .collect::<Vec<_>>();

    // enum definition
    {
        let variants = variants
            .iter()
            .map(AsRef::as_ref)
            .intersperse(",")
            .collect::<String>();
        writeln!(code, "\npub enum {name} {{ {variants} }}").unwrap();
    }

    let match_fn = |code: &mut String,
                    fn_name: &str,
                    return_type: &str,
                    branch: fn(&str, [u8; 2]) -> String| {
        writeln!(
            code,
            "pub fn {fn_name}(&self) -> {return_type} {{ match self {{ {} }} }}",
            variants
                .iter()
                .zip(&names)
                .zip(&stats)
                .map(|((variant, name), stat)| format!(
                    "Self::{} => {},",
                    variant,
                    branch(name, *stat)
                ))
                .collect::<String>()
        )
        .unwrap();
    };

    // impl Name
    writeln!(code, "impl Name for {name} {{").unwrap();

    match_fn(&mut code, "name", "&'static str", |name, _| {
        format!("\"{}\"", name)
    });

    writeln!(code, "}}").unwrap();

    // impl Price
    writeln!(code, "impl Price for {name} {{").unwrap();

    match_fn(&mut code, "buy_price", "u8", |_, [price, _]| {
        price.to_string()
    });
    match_fn(&mut code, "sell_price", "u8", |_, [_, price]| {
        price.to_string()
    });

    writeln!(code, "}}").unwrap();

    code
}

fn sanitize_variant_name(name: &str) -> String {
    let mut string = name.replace('8', "Eight");
    string.remove_matches([' ', '!', '\'', '-', '.']);
    string
}

struct CodegenJoker {
    variant_name: String,
    actual_name: String,
    rarity: &'static str,
    buy_price: u8,
    sell_price: u8,
    effect_type: JokerEffectType,
    compatibility: JokerCompatibility,
}

async fn jokers() -> String {
    #[derive(Deserialize)]
    struct OuterQuery {
        query: Query,
    }
    #[derive(Deserialize)]
    struct Query {
        categorymembers: Vec<CategoryMember>,
    }
    #[derive(Deserialize)]
    struct CategoryMember {
        title: String,
    }

    #[derive(Deserialize, Debug)]
    struct OuterParser {
        parse: Parse,
    }
    #[derive(Deserialize, Debug)]
    struct Parse {
        properties: Properties,
    }
    #[derive(Deserialize, Debug)]
    struct Properties {
        infoboxes: String,
    }

    #[derive(Deserialize, Debug)]
    struct OuterData {
        data: Vec<Data>,
    }
    #[derive(Deserialize, Debug)]
    struct Data {
        r#type: String,
        data: serde_json::Value,
    }

    // get jokers
    let query: OuterQuery = isahc::get_async(
            "https://balatrogame.fandom.com/api.php?action=query&list=categorymembers&cmtitle=Category:Jokers&cmprop=title|ids&cmlimit=500&format=json&formatversion=2",
        ).await.unwrap().json().await.unwrap();
    let queries = query
            .query
            .categorymembers
            .into_iter()
            .map(|category_member| category_member.title)
            .filter(|title| !title.contains("Category") && title != "Jokers" && title != "Chaos Theory")
            .map(|title| format!("https://balatrogame.fandom.com/api.php?action=parse&page={}&prop=properties&format=json&formatversion=2", utf8_percent_encode(&title,NON_ALPHANUMERIC)))
            .map(|uri| smol::spawn(isahc::get_async(uri))).collect::<Vec<_>>();

    let mut images: Vec<(smol::Task<_>, String)> = Vec::new();

    let mut jokers = Vec::new();

    // for every joker
    for query in queries {
        let parser: OuterParser = query.await.unwrap().json().await.unwrap();
        let str = parser.parse.properties.infoboxes;

        let mut variant_name = None;
        let mut actual_name = None;
        let mut rarity = None;
        let mut buy_price: Option<u8> = None;
        let mut sell_price: Option<u8> = None;
        let mut effect_type = None;
        let mut compatibility = None;

        let parsed: [OuterData; 1] = serde_json::from_str(&str).unwrap();
        for data in &parsed[0].data {
            match data.r#type.as_str() {
                "title" => {
                    let string = data.data.get("value").unwrap().as_str().unwrap();

                    variant_name = Some(sanitize_variant_name(string));
                    actual_name = Some(string.to_string());
                }
                "image" => {
                    images.push((
                        smol::spawn(isahc::get_async(
                            data.data.as_array().unwrap()[0]
                                .get("url")
                                .unwrap()
                                .as_str()
                                .unwrap(),
                        )),
                        actual_name.clone().unwrap(),
                    ));
                }
                "group" => {
                    let values: Vec<Data> =
                        serde_json::from_value(data.data.get("value").unwrap().clone()).unwrap();

                    assert!(&values[0].r#type == "header");
                    assert!(values.iter().skip(1).all(|data| &data.r#type == "data"));

                    match values[0].data.get("value").unwrap().as_str().unwrap() {
                        "Rarity" => {
                            let value = values[1].data.get("value").unwrap().as_str().unwrap();
                            let rarities = ["Common", "Uncommon", "Rare", "Legendary"];
                            for rarity_str in rarities {
                                if value.contains(rarity_str) {
                                    rarity = Some(rarity_str)
                                }
                            }
                        }
                        "Stats" => {
                            #[derive(Deserialize)]
                            struct Stat {
                                label: String,
                                value: String,
                            }

                            for value in values.iter().skip(1) {
                                let stat: Stat =
                                    serde_json::from_value(value.data.clone()).unwrap();
                                match stat.label.as_str() {
                                    "Buy Price" => {
                                        buy_price = Some(
                                            stat.value
                                                .chars()
                                                .filter(|char| char.is_numeric())
                                                .collect::<String>()
                                                .parse()
                                                .unwrap(),
                                        )
                                    }
                                    "Sell Price" => {
                                        sell_price = Some(
                                            stat.value
                                                .split(' ')
                                                .next_back()
                                                .unwrap()
                                                .parse()
                                                .unwrap(),
                                        )
                                    }
                                    "Type" => {
                                        effect_type =
                                            Some(if stat.value.contains("Additive Mult") {
                                                JokerEffectType::new(
                                                    false, true, false, false, false, false,
                                                )
                                            } else if stat.value.contains("Chips") {
                                                JokerEffectType::new(
                                                    true, false, false, false, false, false,
                                                )
                                            } else if stat.value.contains("Multiplicative Mult") {
                                                JokerEffectType::new(
                                                    false, false, true, false, false, false,
                                                )
                                            } else if stat.value.contains("Chips and Additive Mult")
                                            {
                                                JokerEffectType::new(
                                                    true, true, false, false, false, false,
                                                )
                                            } else if stat.value.contains("Effect") {
                                                JokerEffectType::new(
                                                    false, false, false, true, false, false,
                                                )
                                            } else if stat.value.contains("Retrigger") {
                                                JokerEffectType::new(
                                                    false, false, false, false, true, false,
                                                )
                                            } else if stat.value.contains("Economy") {
                                                JokerEffectType::new(
                                                    false, false, false, false, false, true,
                                                )
                                            } else {
                                                unreachable!()
                                            });
                                    }
                                    "Activation" => {}
                                    other => panic!("{other}"),
                                }
                            }
                        }
                        "Compatibility" => {
                            #[derive(Deserialize)]
                            struct Compatibility {
                                value: String,
                                source: String,
                            }
                            let [mut copyable, mut perishable, mut eternal] = [None; 3];

                            for value in &values[1..] {
                                let parsed_compatibility: Compatibility =
                                    serde_json::from_value(value.data.clone()).unwrap();

                                let value = parsed_compatibility.value.contains("Yes");
                                match parsed_compatibility.source.as_str() {
                                    "compat-copyable" => copyable = Some(value),
                                    "compat-perishable" => perishable = Some(value),
                                    "compat-eternal" => eternal = Some(value),
                                    other => panic!("{other}"),
                                }
                            }

                            let [copyable, perishable, eternal] =
                                [copyable, perishable, eternal].map(Option::unwrap);

                            compatibility = Some(JokerCompatibility {
                                copyable,
                                perishable,
                                eternal,
                            })
                        }
                        "Effect" | "Unlock Requirement" => {}
                        other => panic!("{other}"),
                    }
                }
                other => panic!("{other}"),
            }
        }

        jokers.push(CodegenJoker {
            variant_name: variant_name.unwrap(),
            actual_name: actual_name.unwrap(),
            rarity: rarity.unwrap(),
            buy_price: buy_price.unwrap(),
            sell_price: sell_price.unwrap(),
            effect_type: effect_type.unwrap(),
            compatibility: compatibility.unwrap(),
        });
    }

    let string = jokers_codegen(jokers).await;
    handle_assets(images).await;

    string
}

#[expect(clippy::type_complexity)]
async fn handle_assets(
    // (task, asset name)
    images: Vec<(
        smol::Task<Result<isahc::Response<isahc::AsyncBody>, isahc::Error>>,
        String,
    )>,
) {
    let assets = ROOT_DIR.join("assets");
    let _ = fs::create_dir(&assets).await; // ignore already exists error
    for (image, name) in images {
        let image = image.await.unwrap().bytes().await.unwrap();

        let mut path = assets.join(name);
        path.set_extension("png");

        fs::write(path, image).await.unwrap();
    }
}

async fn jokers_codegen(jokers: Vec<CodegenJoker>) -> String {
    let mut code = String::new();

    // enum definition
    {
        let variants = jokers
            .iter()
            .map(|joker| joker.variant_name.as_str())
            .intersperse(",")
            .collect::<String>();
        writeln!(code, "\npub enum JokerType {{ {variants} }}").unwrap();
    }

    let mut match_fn = |code: &mut String,
                        fn_name: &str,
                        return_type: &str,
                        branch: fn(&CodegenJoker) -> String| {
        writeln!(
            code,
            "pub fn {fn_name}(&self) -> {return_type} {{ match self {{ {} }} }}",
            jokers
                .iter()
                .map(|joker| format!("Self::{} => {},", joker.variant_name, branch(joker)))
                .collect::<String>()
        )
        .unwrap();
    };

    // impl block start
    writeln!(code, "impl JokerType {{").unwrap();

    match_fn(&mut code, "rarity", "Rarity", |joker| {
        format!("Rarity::{}", joker.rarity)
    });
    match_fn(&mut code, "effect_type", "JokerEffectType", |joker| {
        let et = &joker.effect_type;
        format!(
            "JokerEffectType::new({}, {}, {}, {}, {}, {})",
            et.chips, et.add_mult, et.mult_mult, et.effect, et.retrigger, et.economy
        )
    });
    match_fn(&mut code, "compatibility", "JokerCompatibility", |joker| {
        let c = &joker.compatibility;
        format!(
            "JokerCompatibility::new({}, {}, {})",
            c.copyable, c.perishable, c.eternal
        )
    });

    // impl block end
    writeln!(code, "}}").unwrap();

    // impl Name
    writeln!(code, "impl Name for JokerType {{").unwrap();

    match_fn(&mut code, "name", "&'static str", |joker| {
        format!("\"{}\"", joker.actual_name)
    });

    writeln!(code, "}}").unwrap();

    // impl Price
    writeln!(code, "impl Name for JokerType {{").unwrap();

    match_fn(&mut code, "buy_price", "u8", |joker| {
        joker.buy_price.to_string()
    });
    match_fn(&mut code, "sell_price", "u8", |joker| {
        joker.sell_price.to_string()
    });

    writeln!(code, "}}").unwrap();

    code
}
