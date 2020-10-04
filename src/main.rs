use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader, Read, Write};


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

fn run(source: &str) {
    println!("{}", source);
}

fn main() {
    match env::args().nth(1) {
        Some(file) => {
            let mut source = String::new();
            File::open(file)
                .and_then(|mut f| f.read_to_string(&mut source))
                .expect("read source file failed");
            run(&source);
        }
        None => run_prompt(),
    }
    println!("Bye Bye!");
}
