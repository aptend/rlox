use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read, Write};

// use rlox::ast::ast_printer::AstPrint;
use rlox::interpreter::Interpreter;
use rlox::parser::Parser;
use rlox::scanner::Scanner;

fn run_prompt() {
    let mut lines = BufReader::new(io::stdin()).lines();
    let mut stdout = io::stdout();
    loop {
        // prompt
        write!(&mut stdout, "> ").expect("write stdout failed");
        stdout.flush().unwrap();
        // read line
        if let Some(line) = lines.next() {
            run(&line.expect("read stdin failed"));
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
    run(&source);
}

fn run(source: &str) {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);
    let stmts = parser.parse();
    let interpreter = Interpreter::new();
    match stmts {
        Ok(stmts) => if let Err(err) = interpreter.interpret(&stmts) {
            println!("{}", err);
        },
        Err(errs) => {
            for e in errs {
                println!("{}", e);
            }
        }
    }
}

fn main() {
    match env::args().nth(1) {
        Some(file) => run_file(file),
        None => run_prompt(),
    }
    println!("Bye Bye!");
}
