mod lexer;
mod parser;
mod visitors;

use lexer::Lexer;

const TEST: &str = r#"
i 10 set
label loop
    i print
    i i 1 - set
    i ?goto loop
"#;

fn main() {
    let lexer = Lexer::from_source(TEST);

    let prog = parser::parse(lexer)
        .unwrap();

    println!("labels: {:?}", prog.1);
    println!("expressions: {:?}", prog.0);

    visitors::eval::run(prog);
}
