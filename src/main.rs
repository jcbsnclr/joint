mod lexer;
mod parser;
mod visitors;

use lexer::Lexer;

const TEST: &str = r#"
decl i
decl f1
decl f2
decl f3

f1 0 set
f2 1 set

f1 print f2 print

do
    f3 
        f1 f2 +
    set

    f3 print

    f1 f2 set
    f2 f3 set

    i i 1 + set 
i 10 = not ?loop
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
