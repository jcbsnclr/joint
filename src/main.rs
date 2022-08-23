mod lexer;
mod parser;
mod visitors;

use lexer::Lexer;

const TEST: &str = r#"
decl cur
cur 2 set

1 ?goto main

label print_pows2
    cur print
    label do_pow2
    cur cur 2 * set
    cur 256 = not ?goto print_pows2
    return

label main
    call print_pows2
"#;

fn main() {
    let lexer = Lexer::from_source(TEST);

    let prog = parser::parse(lexer)
        .unwrap();

    println!("expressions: {:?}", prog);

    let ir = visitors::compiler::compile(prog);

    println!("IR: {:?}", ir);

    visitors::eval::run(ir);
}
