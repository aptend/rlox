use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read, Write};

use rlox::interpreter::Interpreter;
use rlox::parser::Parser;
use rlox::resolver::Resolver;
use rlox::scanner::Scanner;

fn run_prompt() {
    let mut lines = BufReader::new(io::stdin()).lines();
    let mut stdout = io::stdout();
    let mut interpreter = Interpreter::new();
    loop {
        // prompt
        write!(&mut stdout, "> ").expect("write stdout failed");
        stdout.flush().unwrap();
        // read line
        if let Some(line) = lines.next() {
            run(&line.expect("read stdin failed"), &mut interpreter);
        } else {
            break;
        }
    }
}

fn run_file(filename: String) {
    let mut source = String::new();
    File::open(filename)
        .and_then(|mut f| f.read_to_string(&mut source))
        .expect("read source file failed");
    let mut interpreter = Interpreter::new();
    run(&source, &mut interpreter);
}

fn run(source: &str, interpreter: &mut Interpreter) {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);
    let stmts = match parser.parse() {
        Ok(s) => s,
        Err(ref errs) => {
            for e in errs {
                println!("{}", e);
            }
            return;
        }
    };
    let mut resolver = Resolver::new(interpreter);
    if let Err(errs) = resolver.resolve(&stmts) {
        for e in errs {
            println!("{}", e);
        }
        return;
    }
    if let Err(ref e) = interpreter.interpret(&stmts) {
        println!("{}", e);
    }
}

fn main() {
    match env::args().nth(1) {
        Some(file) => run_file(file),
        None => run_prompt(),
    }
    println!("⭐ Bye Bye!⭐");
}
