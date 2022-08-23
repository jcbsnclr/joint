mod lexer;
mod parser;
mod visitors;

use lexer::Lexer;

const TEST: &str = r#"
1 ?goto main

label print_123
    123 print
    return

label main
    call print_123
"#;

fn main() {
    let lexer = Lexer::from_source(TEST);

    let prog = parser::parse(lexer)
        .unwrap();

    println!("labels: {:?}", prog.1);
    println!("expressions: {:?}", prog.0);

    visitors::eval::run(prog);
}
