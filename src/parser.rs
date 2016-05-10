use std::fmt::Debug;
use nom::{multispace, IResult, alpha, alphanumeric};
use nom::IResult::{Done, Error, Incomplete};
use std::str::from_utf8;
use nom::Err::{Code, Node, Position, NodePosition};

// TODO doesn't support exception -

#[derive(Debug, PartialEq, Clone)]
pub struct Parser {
    items: Vec<Item>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Rule {
    name: String,
    production: Production
}

#[derive(Debug, PartialEq, Clone)]
pub enum Item {
    Comment(String),
    Rule(Rule)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Production {
    Terminal(String),
    Identifier(String),
    Optional(Box<Production>),
    Repetition(Box<Production>),
    Repetition1(Box<Production>),
    Group(Box<Production>),
    Alternation(Vec<Production>),
    Concatenation(Vec<Production>),
    Regex(String),
    Exception(Box<Production>, Box<Production>),
}

fn handle_parse<T: Debug>(res: IResult<&[u8], T>) -> Result<T, String> {
    match res {
        Done(x, tree) => {
            // println!("--- done: {:?}", from_utf8(x));
            if x.len() > 0 {
                Err(format!("Incomplete, didn't consume the following: `{:?}`. Parsed: {:?}",
                            from_utf8(x).unwrap(),
                            tree))
            } else {
                Ok(tree)
            }
        }
        // TODO need to improve errors a ton
        Error(x) => Err(get_error_msg(x)),
        Incomplete(n) => Err(format!("Incomplete: {:?}", n)),
    }
}

fn get_error_msg(res: ::nom::Err<&[u8]>) -> String {
    match res {
        // TODO how to check custom errors
        Position(k, p) => format!("ERROR({:?}): ```{}```", k, from_utf8(p).unwrap()),
        NodePosition(k, p, be) => {
            format!("ERROR({:?}): ```{}```\n{}",
                    k,
                    from_utf8(p).unwrap(),
                    get_error_msg(*be))
        }
        Node(k, be) => format!("ERROR({:?}): ```{}```", k, get_error_msg(*be)),
        Code(k) => format!("ERROR({:?})", k),
    }
}

pub fn nom_parse(input: &str) -> Result<Parser, String> {
    handle_parse(grammar(&input.as_bytes()[..]))
}

named!(identifier<&[u8], String>,
       chain!(first: alpha ~
              rest: many0!(alt!(alphanumeric | tag!("_"))),
              || {
                  let s = rest.into_iter().fold(first.to_vec(), |mut l, x| {
                      l.extend_from_slice(x);
                      l
                  });
                  println!("Identifier: {:?}", from_utf8(s.as_slice()));
                  from_utf8(s.as_slice()).unwrap().to_owned()
              }));

named!(double_not_escaped_seq<&[u8], &[u8]>, take_until_either!(&b"\\\""[..]));
named!(double_escaped_seq,
       alt!(tag!("\\r") | tag!("\\n") | tag!("\\t") | tag!("\\\"") | tag!("\\\\")));

named!(single_not_escaped_seq<&[u8], &[u8]>, take_until_either!(&b"\\'"[..]));
named!(single_escaped_seq,
       alt!(tag!("\\r") | tag!("\\n") | tag!("\\t") | tag!("\\'") | tag!("\\\\")));

named!(single_quote_literal<&[u8], String>,
       chain!(tag!("'") ~
              s: many0!(map_res!(alt!(single_escaped_seq | single_not_escaped_seq),
                                 from_utf8)) ~
              tag!("'"),
              || {
                  s.into_iter().fold(String::new(), |mut accum, slice| {
                      accum.push_str(slice);
                      accum
                  })
              }));

named!(double_quote_literal<&[u8], String>,
       chain!(tag!("\"") ~
              s: many0!(map_res!(alt!(double_escaped_seq | double_not_escaped_seq),
                                 from_utf8)) ~
              tag!("\""),
              || {
                  s.into_iter().fold(String::new(), |mut accum, slice| {
                      accum.push_str(slice);
                      accum
                  })
              }));


named!(terminal<&[u8], String>,
       chain!(t: alt!(single_quote_literal | double_quote_literal),
              || {
                  //println!("Terminal: {:?}", t);
                  t
              }));

named!(lhs<&[u8], String>,
       chain!(ident: identifier,
              || {
                  println!("LHS: {:?}", ident);
                  ident
              }));

named!(group<&[u8], Production>,
       chain!(tag!("(") ~ multispace? ~ production: rhs ~ multispace? ~ tag!(")"),
              || {
                  println!("Group: {:?}", production);
                  Production::Group(Box::new(production))
              }));

named!(repetition<&[u8], Production>,
       chain!(tag!("{") ~ multispace? ~ production: rhs ~ multispace? ~ tag!("}") ~ repetition_exception: tag!("-")?,
              || {
                  println!("Rep: {:?}", production);
                  match repetition_exception {
                      Some(_) => Production::Repetition1(Box::new(production)),
                      None => Production::Repetition(Box::new(production)),
                  }
              }));

named!(optional<&[u8], Production>,
       chain!(tag!("[") ~ multispace? ~ production: rhs ~ multispace? ~ tag!("]"),
              || {
                  println!("Opt: {:?}", production);
                  Production::Optional(Box::new(production))
              }));

named!(regex<&[u8], Production>,
       chain!(tag!("/") ~
              regex: map_res!(escaped!(take_until!("/"), '\\', is_a_bytes!(&b"/"[..])), from_utf8) ~
              tag!("/"),
              || {
                  Production::Regex(regex.to_string())
              }));

named!(production<&[u8], Production>,
       chain!(prod: alt!(chain!(ident: identifier, || {Production::Identifier(ident)})
                         | chain!(t: terminal, || {Production::Terminal(t)})
                         | optional
                         | repetition
                         | group
                         | regex) ~
              exception: chain!(multispace? ~
                                tag!("-") ~
                                multispace? ~
                                except: production,
                                || {except})?,
              || {
                  println!("production: {:?}, {:?}", prod, exception);
                  match exception {
                      Some(ref except) => {
                          Production::Exception(Box::new(prod), Box::new(except.clone()))
                      },
                      None => prod
                  }
              }));

named!(rhs<&[u8], Production>,
       chain!(first: production ~
              rest: many0!(chain!(multispace? ~
                                  separator: map_res!(alt!(tag!("|") | tag!(",")), from_utf8)? ~
                                  multispace? ~
                                  production: production,
                                  || {
                                      (separator, production)
                                  })),
              || {
                  println!("rhs: {:?}, {:?}", first, rest);
                  let mut separator = "".to_string();

                  let productions = rest.into_iter().fold(vec![first.clone()], |mut accum, (sep, prod)| {
                      let assumed_separator = match sep {
                          Some(ref s) => s.to_string(),
                          None => ",".to_string()
                      };

                      if separator == "" {
                          separator = assumed_separator;
                      } else if separator != assumed_separator {
                          // TODO what if it's just space separated
                          println!("FUCK");
                      }
                      accum.push(prod);
                      accum
                  });

                  match &separator[..] {
                      "|" => Production::Alternation(productions),
                      "," => Production::Concatenation(productions),
                      _ => first
                  }
              })
       // alt_complete!(chain!(productions: separated_nonempty_list!(chain!(multispace? ~ tag!("|") ~ multispace?, || {}),
       //                                                            production),
       //                      || {
       //                          println!("Alt: {:?}", productions);
       //                          Production::Alternation(productions)
       //                      })
       //               | chain!(productions: separated_nonempty_list!(chain!(multispace? ~ tag!(",") ~ multispace?, || {}),
       //                                                              production),
       //                        || {
       //                            println!("Concat: {:?}", productions);
       //                            Production::Concatenation(productions)
       //                        })
       //               | production)
       );

named!(rule<&[u8], Item>,
       chain!(name: lhs ~
              multispace? ~
              tag!("=") ~
              multispace? ~
              production: rhs ~
              multispace? ~
              alt!(tag!(";") | tag!(".")) ~
              multispace?,
              || {
                  println!("Rule: {:?}, {:?}", name, production);
                  Item::Rule(
                      Rule {
                          name: name,
                          production: production,
                      }
                  )
              }));

named!(comment<&[u8], Item>,
       chain!(multispace? ~
              tag!("(*") ~
              comment: map_res!(take_until!("*)"), from_utf8) ~
              tag!("*)") ~
              multispace?,
              || {
                  Item::Comment(comment.to_string())
              }));

named!(grammar<&[u8], Parser>,
       chain!(multispace? ~
              items: many1!(alt!(rule | comment)) ~
              multispace?,
              || {
                  println!("Grammar: {:?}", items);
                  Parser {
                      items: items
                  }
              }));

#[test]
fn identifier_works() {
    let res = handle_parse(identifier("Abasekr".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());

    let res = handle_parse(identifier("a".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());

    let res = handle_parse(identifier("A".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
}

#[test]
fn terminal_works() {
    let res = handle_parse(single_quote_literal("'A'".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());

    let res = handle_parse(double_quote_literal("\"A\"".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());

    let res = handle_parse(terminal("\"A\"".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());

    let res = handle_parse(terminal("\";\"".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
}

#[test]
fn rhs_works() {
    let res = handle_parse(rhs("\"A\" | \"B\" | \"C\" | \"D\" | \"E\" | \"F\" | \"G\"".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());

    let res = handle_parse(rhs("letter | digit | symbol | \"_\"".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
}

#[test]
fn rule_works() {
    let res = handle_parse(rule("character = letter | digit | symbol | \"_\" ;".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), Item::Rule(Rule{
        name: "character".to_string(),
        production: Production::Alternation(vec![
            Production::Identifier("letter".to_string()),
            Production::Identifier("digit".to_string()),
            Production::Identifier("symbol".to_string()),
            Production::Terminal("_".to_string()),
        ])
    }));

    let res = handle_parse(rule("identifier = letter , { letter | digit | \"_\" } ;".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), Item::Rule(Rule{
        name: "identifier".to_string(),
        production: Production::Concatenation(vec![
            Production::Identifier("letter".to_string()),
            Production::Repetition(Box::new(Production::Alternation(vec![
                Production::Identifier("letter".to_string()),
                Production::Identifier("digit".to_string()),
                Production::Terminal("_".to_string()),
            ]))),
        ])
    }));

    let res = handle_parse(rule("Cypher = WS AllOptions WS Statements ;".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), Item::Rule(Rule{
        name: "Cypher".to_string(),
        production: Production::Concatenation(vec![
            Production::Identifier("WS".to_string()),
            Production::Identifier("AllOptions".to_string()),
            Production::Identifier("WS".to_string()),
            Production::Identifier("Statements".to_string()),
        ])
    }));

    let res = handle_parse(rule("StringLiteral = ('\", { ANY - ('\"' | '\') | EscapedChar }, '\"')
  | (\"'\", { ANY - (\"'\" | '\') | EscapedChar }, \"'\")
  ;".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), Item::Rule(Rule{
        name: "Cypher".to_string(),
        production: Production::Concatenation(vec![
            Production::Identifier("WS".to_string()),
            Production::Identifier("AllOptions".to_string()),
            Production::Identifier("WS".to_string()),
            Production::Identifier("Statements".to_string()),
        ])
    }));
}

#[test]
fn it_works() {
    // Test parsing it's own grammar
    let res = handle_parse(grammar("letter = \"A\" | \"B\" | \"C\" | \"D\" | \"E\" | \"F\" | \"G\"
       | \"H\" | \"I\" | \"J\" | \"K\" | \"L\" | \"M\" | \"N\"
       | \"O\" | \"P\" | \"Q\" | \"R\" | \"S\" | \"T\" | \"U\"
       | \"V\" | \"W\" | \"X\" | \"Y\" | \"Z\" | \"a\" | \"b\"
       | \"c\" | \"d\" | \"e\" | \"f\" | \"g\" | \"h\" | \"i\"
       | \"j\" | \"k\" | \"l\" | \"m\" | \"n\" | \"o\" | \"p\"
       | \"q\" | \"r\" | \"s\" | \"t\" | \"u\" | \"v\" | \"w\"
       | \"x\" | \"y\" | \"z\" ;
digit = \"0\" | \"1\" | \"2\" | \"3\" | \"4\" | \"5\" | \"6\" | \"7\" | \"8\" | \"9\" ;
symbol = \"[\" | \"]\" | \"{\" | \"}\" | \"(\" | \")\" | \"<\" | \">\"
       | \"'\" | '\"' | \"=\" | \"|\" | \".\" | \",\" | \";\" ;
character = letter | digit | symbol | \"_\" ;

identifier = letter , { letter | digit | \"_\" } ;
terminal = \"'\" , character , { character } , \"'\" 
| '\"' , character , { character } , '\"' ;

lhs = identifier ;
rhs = identifier
| terminal
| \"[\" , rhs , \"]\"
| \"{\" , rhs , \"}\"
| \"(\" , rhs , \")\"
| rhs , \"|\" , rhs
| rhs , \",\" , rhs ;

rule = lhs , \"=\" , rhs , \";\" ;
grammar = { rule } ;".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
}
