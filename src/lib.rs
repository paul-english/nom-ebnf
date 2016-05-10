#[macro_use]
extern crate nom;

mod parser;

use parser::{nom_parse};

fn parse() {
    // TODO
    nom_parse("letter = \"A\" | \"B\" | \"C\" | \"D\" | \"E\" | \"F\" | \"G\" ;");
}

#[test]
fn it_works() {
    let res = nom_parse("letter = \"A\" | \"B\" | \"C\" | \"D\" | \"E\" | \"F\" | \"G\"
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
grammar = { rule } ;");
    println!("- res: {:?}", res);
    assert!(res.is_ok());
}


#[test]
fn reading_opencypher_grammar() {
    use std::io::prelude::*;
    use std::fs::File;

    let mut f = File::open("./src/cypher.ebnf").unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();

    let res = nom_parse(&s[..]);
    println!("- opencypher parse: {:?}", res);
    assert!(res.is_ok());
}
