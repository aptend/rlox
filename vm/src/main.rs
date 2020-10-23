use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read, Write};

use vm::compiler::Compiler;
use vm::scanner::Scanner;
use vm::Machine;

fn run_prompt() {
    let mut lines = BufReader::new(io::stdin()).lines();
    let mut stdout = io::stdout();
    // let mut interpreter = Interpreter::new();
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
    let mut compiler = Compiler::new(scanner, "EVA-01 Test Type");
    let (chunk, arena) = match compiler.compile() {
        Ok(chunk) => chunk,
        Err(ref errs) => {
            for err in errs {
                println!("{}", err);
            }
            return;
        }
    };
    chunk.disassemble();
    let mut vm = Machine::new(&chunk, arena);
    if let Err(ref e) = vm.run() {
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
