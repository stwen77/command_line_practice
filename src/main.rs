extern crate logos;

mod lexer;
mod parser;

use crate::logos::internal::LexerInternal;
use crate::logos::Logos;
use lexer::Token;
use parser::parse;

use std::fs::File;
use std::io::prelude::*;

fn main() {
    let test_file: &str = "./test.nb";
    let mut f = File::open(test_file).unwrap();
    let mut f_string = String::new();
    f.read_to_string(&mut f_string).unwrap();
    let mut pre_lexer = Token::lexer(f_string.as_str());

    let mut lexer = vec![];
    while pre_lexer.token != Token::End {
        lexer.push((pre_lexer.token, pre_lexer.slice()));
        println!(
            "token: {:?}, slice: {:?}",
            pre_lexer.token,
            pre_lexer.slice()
        );
        pre_lexer.advance();
    }
    parse(&mut lexer);
}
