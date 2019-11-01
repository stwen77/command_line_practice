extern crate logos;

mod lexer;

use crate::logos::Logos;
use lexer::Token;

fn main() {
    let test_str: &str = " let x = \"abcd\" ";
    let mut lexer = Token::lexer(test_str);

    while lexer.token != Token::End {
        println!("token: {:?}, slice: {}", lexer.token, lexer.slice());
        lexer.advance();
    }
}
