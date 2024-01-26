fn main() -> anyhow::Result<()> {
    let mut settings = rustemo_compiler::Settings::new()
        .in_source_tree()
        .force(true)
        .dot(true)
        .table_type(rustemo_compiler::TableType::LALR_RN)
        .parser_algo(rustemo_compiler::ParserAlgo::GLR);
    if std::env::var("CARGO_FEATURE_ARRAYS").is_ok() {
        settings = settings.generator_table_type(rustemo_compiler::GeneratorTableType::Arrays);
    }

    settings.process_dir().map_err(|x| anyhow::anyhow!(x))?;
    Ok(())
}
