#![feature(iter_intersperse)]
#![feature(exit_status_error)]
#![feature(string_remove_matches)]

use balatro::{JokerCompatibility, JokerEffectType};
use isahc::AsyncReadResponseExt;
use percent_encoding::{NON_ALPHANUMERIC, utf8_percent_encode};
use serde::Deserialize;
use smol::fs;
use std::process::Command;
use std::{convert::Infallible, env, path::PathBuf};

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
    properties: Vec<serde_json::Value>,
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

fn main() {
    smol::block_on(async {
        // get jokers
        let query: OuterQuery = isahc::get_async(
            "https://balatrogame.fandom.com/api.php?action=query&list=categorymembers&cmtitle=Category:Jokers&cmprop=title|ids&cmlimit=500&format=json",
        ).await.unwrap().json().await.unwrap();
        let queries = query
            .query
            .categorymembers
            .into_iter()
            .map(|category_member| category_member.title)
            .filter(|title| !title.contains("Category") && title != "Jokers" && title != "Chaos Theory")
            .map(|title| format!("https://balatrogame.fandom.com/api.php?action=parse&page={}&prop=properties&format=json", utf8_percent_encode(&title,NON_ALPHANUMERIC).to_string()))
            .map(|uri| smol::spawn(isahc::get_async(uri))).collect::<Vec<_>>();

        let mut images: Vec<(smol::Task<_>, String)> = Vec::new();

        struct Joker {
            name: String,
            rarity: &'static str,
            buy_price: u8,
            sell_price: u8,
            effect_type: JokerEffectType,
            compatibility: JokerCompatibility,
        }

        let mut jokers = Vec::new();

        // for every joker
        for query in queries {
            let parser: OuterParser = query.await.unwrap().json().await.unwrap();
            let str = parser.parse.properties[0]
                .get("*")
                .unwrap()
                .as_str()
                .unwrap();

            let mut name = None;
            let mut rarity = None;
            let mut buy_price: Option<u8> = None;
            let mut sell_price: Option<u8> = None;
            let mut effect_type = None;
            let mut compatibility = None;

            let parsed: [OuterData; 1] = serde_json::from_str(str).unwrap();
            for data in &parsed[0].data {
                match data.r#type.as_str() {
                    "title" => {
                        let mut string = data
                            .data
                            .get("value")
                            .unwrap()
                            .as_str()
                            .unwrap()
                            // rust compatibility
                            .replace('8', "Eight");

                        string.remove_matches([' ', '!', '\'', '-', '.']);

                        name = Some(string);
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
                            name.clone().unwrap(),
                        ));
                    }
                    "group" => {
                        let values: Vec<Data> =
                            serde_json::from_value(data.data.get("value").unwrap().clone())
                                .unwrap();

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
                                                } else if stat.value.contains("Multiplicative Mult")
                                                {
                                                    JokerEffectType::new(
                                                        false, false, true, false, false, false,
                                                    )
                                                } else if stat
                                                    .value
                                                    .contains("Chips and Additive Mult")
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

            jokers.push(Joker {
                name: name.unwrap(),
                rarity: rarity.unwrap(),
                buy_price: buy_price.unwrap(),
                sell_price: sell_price.unwrap(),
                effect_type: effect_type.unwrap(),
                compatibility: compatibility.unwrap(),
            });
        }

        let root_dir = {
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
        };

        // handle codegen
        let lib_path = root_dir.join("balatro/src/lib.rs");

        // generate code
        let variants = jokers
            .iter()
            .map(|joker| joker.name.as_str())
            .intersperse(",")
            .collect::<String>();
        let enum_string = format!("\npub enum JokerType {{ {variants} }}");

        // write code
        const CODEGEN_START: &str = "// CODEGEN START";
        const CODEGEN_END: &str = "// CODEGEN END";

        let mut lib_string = fs::read_to_string(&lib_path).await.unwrap();

        let start_codegen = lib_string.find(CODEGEN_START).unwrap() + CODEGEN_START.len();
        let end_codegen = lib_string.find(CODEGEN_END).unwrap() - 1;

        lib_string.replace_range(start_codegen..end_codegen, &enum_string);

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

        // handle fetched assets
        let assets = root_dir.join("assets");
        let _ = fs::create_dir(&assets).await; // ignore already exists error
        for (image, name) in images {
            let image = image.await.unwrap().bytes().await.unwrap();

            let mut path = assets.join(name);
            path.set_extension("png");

            fs::write(path, image).await.unwrap();
        }
    });
}
