fn main() -> anyhow::Result<()> {
    vcltl();
    if let Ok(var) = std::env::var("DEBUG") {
        if let Ok(false) = var.parse::<bool>() {
            drop_windows_stuff();
        }
    }


    let mut settings = rustemo_compiler::Settings::new()
        .in_source_tree()
        .force(true)
        .dot(true)
        // .table_type(rustemo_compiler::TableType::LALR_RN)
        .parser_algo(rustemo_compiler::ParserAlgo::GLR);
    if std::env::var("CARGO_FEATURE_ARRAYS").is_ok() {
        settings = settings.generator_table_type(rustemo_compiler::GeneratorTableType::Arrays);
    }

    settings.process_dir().map_err(|x| anyhow::anyhow!(x))?;
    Ok(())
}


fn vcltl() {
    if std::env::var("CARGO_CFG_WINDOWS").is_err() {
        println!(
            "cargo:warning=VC-LTL only supports Windows host. So the VC-LTL stage is skipped."
        );
        return;
    }

    let target_os =
        std::env::var("CARGO_CFG_TARGET_OS").expect("CARGO_CFG_TARGET_OS should be set by cargo.");
    let target_env = std::env::var("CARGO_CFG_TARGET_ENV")
        .expect("CARGO_CFG_TARGET_ENV should be set by cargo.");

    if !target_os.contains("windows") {
        println!(
            "cargo:warning=VC-LTL only supports Windows target. So the VC-LTL stage is skipped."
        );
        return;
    }

    if !target_env.contains("msvc") {
        println!(
            "cargo:warning=VC-LTL only supports MSVC toolchain. So the VC-LTL stage is skipped."
        );
        return;
    }

    let vcltl_root = std::env::var("VCLTL_ROOT")
        .or_else(|_| std::env::var("CARGO_MANIFEST_DIR"))
        .expect("VCLTL_ROOT or CARGO_MANIFEST_DIR should be set.");
    let target_arch = std::env::var("CARGO_CFG_TARGET_ARCH")
        .expect("CARGO_CFG_TARGET_ARCH should be set by cargo.");

    let version = match std::env::var("VCLTL_VERSION") {
        Ok(version) => version.parse::<usize>().unwrap(),
        _ => 2600,
    };

    let (target_platform, version) = match (
        target_arch.contains("x86_64"),
        target_arch.contains("x86"),
        target_arch.contains("arm"),
        target_arch.contains("aarch64"),
    ) {
        (true, _, _, _) => ("x64", version.max(3790)),
        (_, true, _, _) => ("Win32", version.max(2600)),
        (_, _, true, _) => ("ARM", version.max(9200)),
        (_, _, _, true) => ("ARM64", version.max(10240)),
        _ => {
            println!(
                "cargo:warning=VC-LTL does not support {} platform. So the VC-LTL stage is \
                 skipped.",
                target_arch
            );
            return;
        }
    };

    let version_string = if version >= 19041 {
        "10.0.19041.0"
    } else if version >= 10240 {
        "10.0.10240.0"
    } else if version >= 9200 {
        "6.2.9200.0"
    } else if version >= 6000 {
        "6.0.6000.0"
    } else if version >= 3790 {
        "5.2.3790.0"
    } else {
        "5.1.2600.0"
    };

    let library_folder = &format!(
        "{}/TargetPlatform/{}/lib/{}",
        vcltl_root, version_string, target_platform
    );

    if !std::path::Path::new(library_folder).exists() {
        println!("cargo:warning=VC-LTL can't find lib files, please download the binary files from https://github.com/Chuyu-Team/VC-LTL/releases/latest. So the VC-LTL stage is skipped.");
        return;
    }

    for line in [
        "#######################################################################".to_string(),
        "#                                                                     #".to_string(),
        "#     *         *      * *             *        * * * * *  *          #".to_string(),
        "#      *       *     *                 *            *      *          #".to_string(),
        "#       *     *     *       * * * * *  *            *      *          #".to_string(),
        "#        *   *       *                 *            *      *          #".to_string(),
        "#          *           * *             * * * *      *      * * * *    #".to_string(),
        "#                                                                     #".to_string(),
        "#######################################################################".to_string(),
        format!("VC-LTL Path : {}", vcltl_root),
        format!("WindowsTargetPlatformMinVersion : {}", version_string),
        format!("Platform : {}", target_platform),
    ] {
        println!("cargo:warning={line}");
    }

    println!("cargo:rustc-link-search={}", library_folder);
}

fn drop_windows_stuff() {
    if std::env::var("CARGO_CFG_WINDOWS").is_err() {
        return;
    }

    // File alignment flags to reduce size of `.text` section.
    //   println!("cargo:rustc-link-arg=/ALIGN:128");
    //   println!("cargo:rustc-link-arg=/FILEALIGN:128");
    // Merges empty `.rdata` and `.pdata` into .text section saving a few bytes in
    // data directories portion  of PE header.
    println!("cargo:rustc-link-arg=/MERGE:.rdata=.text");
    println!("cargo:rustc-link-arg=/MERGE:.pdata=.text");
    // Removes `IMAGE_DEBUG_DIRECTORY` from PE.
    // println!("cargo:rustc-link-arg=/EMITPOGOPHASEINFO");
    // println!("cargo:rustc-link-arg=/DEBUG:NONE");
    // See: https://github.com/mcountryman/min-sized-rust-windows/pull/7
    println!("cargo:rustc-link-arg=/STUB:stub.exe");
}
