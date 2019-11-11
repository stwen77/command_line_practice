extern crate logos;

mod any;
mod engine;
mod lexer;
mod parser;

use crate::logos::Logos;
use engine::*;
use lexer::Token;
use parser::parse;

use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut engine_vm = Engine::new();
    engine_vm.print_engine();
    engine_vm.register_fn("test", test_fn);
    let (mut a, mut b, mut c) = (1, 2, 3);
    engine_vm.call_fn("test".to_string(), vec![&mut a, &mut b, &mut c]);
    println!("Hello, world!");

    for fname in env::args().skip(1) {
        engine_vm.eval_file::<()>(&fname);
    }
}

fn test_lexer_and_parser() {
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

fn test_fn(a: i32, b: i32, c: i32) {
    println!("test");
}
