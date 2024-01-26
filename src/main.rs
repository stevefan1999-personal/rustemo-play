use grammar_c::{DefaultBuilder, GrammarCParser};
use rustemo::Parser;
use miette::miette;

#[rustfmt::skip]
mod grammar_c;
#[allow(unused)]
#[rustfmt::skip]
mod grammar_c_actions;


fn main() -> miette::Result<()> {
    let forest = GrammarCParser::new()
        .parse(
            r#"

int main() {
    if (rand() % 2 == 0) {
        goto fail;
    }
    goto success;
fail:
    return 1;
success:
    return 0;
}

"#,
        )
        .map_err(|e| miette!("parse error: {}", e))?;

    println!("solutions: {}", forest.solutions());

    for tree in forest {
        let translation_unit = stacker::maybe_grow(4096 * 1024 * 1024, 16384 * 1024 * 1024, || {
            let mut builder = DefaultBuilder::new();
            tree.build(&mut builder)
        });
        println!("{:#?}", translation_unit);
    }

    Ok(())
}
