use std::{env, path::PathBuf};

use isahc::AsyncReadResponseExt;
use serde::Deserialize;
use smol::fs;

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
    properties: [serde_json::Value; 1],
}

#[derive(Deserialize)]
struct Parsed {
    data: Vec<Data>,
}
#[derive(Deserialize)]
struct Data {
    r#type: String,
    data: serde_json::Value,
}

fn main() {
    smol::block_on(async {
        let query: OuterQuery = isahc::get_async(
            "https://balatrogame.fandom.com/api.php?action=query&list=categorymembers&cmtitle=Category:Jokers&cmprop=title|ids&cmlimit=500&format=json",
        ).await.unwrap().json().await.unwrap();
        let queries = query
            .query
            .categorymembers
            .into_iter()
            .map(|category_member| category_member.title)
            .filter(|title| !title.contains("Category") && title != "Jokers")
            .map(|title| title.replace(' ', "%20"))
            .map(|title| smol::spawn(isahc::get_async(format!("https://balatrogame.fandom.com/api.php?action=parse&page={title}&prop=properties&format=json")))).collect::<Vec<_>>();

        let mut images_to_fetch: Vec<(smol::Task<_>, String)> = Vec::new();

        for query in queries {
            let parser: OuterParser = query.await.unwrap().json().await.unwrap();
            let str = parser.parse.properties[0]
                .get("*")
                .unwrap()
                .as_str()
                .unwrap();
            let parsed: Parsed = serde_json::from_str(str).unwrap();
            for data in parsed.data {
                let mut name = None;
                let mut rarity = None;
                let mut buy_price: Option<u8> = None;
                let mut sell_price: Option<u8> = None;
                // the args to JokerEffect::new()
                let mut r#type = None;

                match data.r#type.as_str() {
                    "title" => {
                        name = Some(
                            data.data
                                .get("value")
                                .unwrap()
                                .as_str()
                                .unwrap()
                                .replace(' ', ""),
                        )
                    }
                    "image" => {
                        images_to_fetch.push((
                            smol::spawn(isahc::get_async(
                                data.data.get("url").unwrap().as_str().unwrap(),
                            )),
                            name.unwrap(),
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
                                                    .split(' ')
                                                    .next_back()
                                                    .unwrap()
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
                                            r#type =
                                                Some(if stat.value.contains("Additive Mult") {
                                                    (false, true, false, false, false, false)
                                                } else if stat.value.contains("Chips") {
                                                    (true, false, false, false, false, false)
                                                } else if stat.value.contains("Multiplicative Mult")
                                                {
                                                    (false, false, true, false, false, false)
                                                } else if stat
                                                    .value
                                                    .contains("Chips and Additive Mult")
                                                {
                                                    (true, true, false, false, false, false)
                                                } else if stat.value.contains("Effect") {
                                                    (false, false, false, true, false, false)
                                                } else if stat.value.contains("Retrigger") {
                                                    (false, false, false, false, true, false)
                                                } else if stat.value.contains("Economy") {
                                                    (false, false, false, false, false, true)
                                                } else {
                                                    unreachable!()
                                                });
                                        }
                                        // TODO
                                        "Activation" => {}
                                        other => panic!("{other}"),
                                    }
                                }
                            }
                            "Compatibility" => todo!(),
                            "Effect" | "Unlock Requirement" => {}
                            other => panic!("{other}"),
                        }
                    }
                    other => panic!("{other}"),
                }
            }
        }

        // handle fetched assets
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
        let assets = root_dir.join("assets");
        fs::create_dir(&assets).await.unwrap();
        for (image, name) in images_to_fetch {
            let image = image.await.unwrap().bytes().await.unwrap();
            fs::write(assets.join(name), image).await.unwrap();
        }
    });
}
