mod lexer;
mod parser;
mod visitors;

use structopt::StructOpt;

use std::path::PathBuf;

use lexer::Lexer;

/// Command line aguments
#[derive(structopt::StructOpt)]
#[structopt(name = "turbo")]
struct Cmdline {
    #[structopt(short, long)]
    input: PathBuf,
}

fn main() {
    // Read input file to string
    let cmdline = Cmdline::from_args();
    let source = std::fs::read_to_string(cmdline.input)
        .unwrap();

    // Initialise lexer with source string and pass to parser
    let mut lexer = Lexer::from_source(&source);

    let mut prog = parser::parse(&mut lexer)
        .unwrap();

    // Validate and attach type information 
    visitors::validator::validate(&mut prog);

    println!("{:#?}", prog);

    // Compile to stack-based IR (see: visitors::compiler::IrOp)
    let object = visitors::compiler::compile(prog);

    println!("{:?}", object);

    // Evaluate generated IR 
    visitors::eval::run(object);
}
