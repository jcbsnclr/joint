mod lexer;
mod parser;
mod visitors;

use structopt::StructOpt;

use std::path::PathBuf;

use lexer::Lexer;

#[derive(structopt::StructOpt)]
#[structopt(name = "turbo")]
struct Cmdline {
    #[structopt(short, long)]
    input: PathBuf,
}

fn main() {
    let cmdline = Cmdline::from_args();
    let source = std::fs::read_to_string(cmdline.input)
        .unwrap();

    let mut lexer = Lexer::from_source(&source);

    // let prog = parser::parse_expr(&mut lexer)
    //     .unwrap();

    // println!("expressions: {:?}", prog);

    // let ir = visitors::compiler::compile(prog);

    // println!("IR: {:?}", ir);

    // visitors::eval::run(ir);

    let mut prog = parser::parse(&mut lexer)
        .unwrap();

    visitors::validator::validate(&mut prog);

    println!("{:#?}", prog);

    let object = visitors::compiler::compile(prog);

    println!("{:?}", object);

    visitors::eval::run(object);
}
