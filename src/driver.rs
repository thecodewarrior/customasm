use asm::AssemblerState;
use asm::BinaryOutput;
use diagn::RcReport;
use getopts;
use std::io::{stdout, Read, Write};
use util::enable_windows_ansi_support;
use util::FileServer;

enum OutputFormat {
    Binary,
    BinStr,
    HexStr,
    BinDump,
    HexDump,
}

pub fn drive(args: &Vec<String>, fileserver: &mut FileServer) -> Result<(), ()> {
    let opts = make_opts();

    let report = RcReport::new();

    let result = drive_inner(report.clone(), &opts, args, fileserver);

    if report.has_messages() {
        println!("");
    }

    enable_windows_ansi_support();
    report.print_all(&mut stdout(), fileserver);

    if let Err(show_usage) = result {
        if show_usage {
            print_usage(&opts);
        }
    }

    result.map_err(|_| ())
}

fn drive_inner(
    report: RcReport,
    opts: &getopts::Options,
    args: &Vec<String>,
    fileserver: &mut FileServer,
) -> Result<(), bool> {
    let matches = parse_opts(report.clone(), opts, args).map_err(|_| true)?;

    if matches.opt_present("h") {
        print_usage(&opts);
        return Ok(());
    }

    if matches.opt_present("v") {
        print_version();
        return Ok(());
    }

    let quiet = matches.opt_present("q");
    let out_stdout = matches.opt_present("p");

    let out_format = match matches.opt_str("f").as_ref().map(|s| s.as_ref()) {
        Some("binstr") => OutputFormat::BinStr,
        Some("bindump") => OutputFormat::BinDump,
        Some("hexstr") => OutputFormat::HexStr,
        Some("hexdump") => OutputFormat::HexDump,
        Some("binary") => OutputFormat::Binary,

        None => {
            if out_stdout {
                OutputFormat::HexDump
            } else {
                OutputFormat::Binary
            }
        }

        Some(_) => {
            report.error("invalid output format");
            return Err(true);
        }
    };

    if matches.free.len() < 1 {
        return Err(true);
    }

    let main_asm_file = matches.free[0].clone();

    let output_file = match matches.opt_str("o") {
        Some(f) => f,
        None => match get_default_output_filename(report.clone(), &main_asm_file) {
            Ok(f) => f,
            Err(_) => return Err(true),
        },
    };

    let mut filenames = matches.opt_strs("i");
    let mut flame_name = matches.opt_str("flame");

    for filename in matches.free {
        filenames.push(filename);
    }

    let _assemble_flame = flame::start_guard("assemble");
    let assembled = assemble(report.clone(), fileserver, &filenames, quiet).map_err(|_| false)?;
    drop(_assemble_flame);

    let _format_output_flame = flame::start_guard("format output");
    let output_data = match out_format {
        OutputFormat::BinStr => assembled
            .generate_binstr(0, assembled.len())
            .bytes()
            .collect::<Vec<u8>>(),
        OutputFormat::BinDump => assembled
            .generate_bindump(0, assembled.len())
            .bytes()
            .collect::<Vec<u8>>(),
        OutputFormat::HexStr => assembled
            .generate_hexstr(0, assembled.len())
            .bytes()
            .collect::<Vec<u8>>(),
        OutputFormat::HexDump => assembled
            .generate_hexdump(0, assembled.len())
            .bytes()
            .collect::<Vec<u8>>(),
        OutputFormat::Binary => assembled.generate_binary(0, assembled.len()),
    };
    drop(_format_output_flame);

    let _write_output_flame = flame::start_guard("write output");
    if out_stdout {
        if !quiet {
            println!("success");
            println!("");
        }

        println!("{}", String::from_utf8_lossy(&output_data));
    } else {
        print!("writing `{}`...", &output_file);
        std::io::stdout().flush();
        fileserver
            .write_bytes(report.clone(), &output_file, &output_data, None)
            .map_err(|_| false)?;

        if !quiet {
            println!("success");
        }
    }
    drop(_write_output_flame);

    if let Some(filename) = flame_name {
        use std::fs::File;
        print!("writing flame `{}`...", &filename);
        std::io::stdout().flush();

        let mut file = File::create(&filename).unwrap();
        if filename.to_lowercase().ends_with("html") {
            flame::dump_html(&mut file).unwrap();
            let mut buf = String::new();
            file.read_to_string(&mut buf);
            buf = buf.replace("var width = document.body.offsetWidth;", "var width = document.body.offsetWidth - 20;");
            buf = buf.replace("padding: 0;", "padding: 0 10;");
            file.write(buf.as_bytes()).map_err(|_| ());
        } else if filename.to_lowercase().ends_with("json") {
            flame::dump_json(&mut file).unwrap();
        } else {
            flame::dump_text_to_writer(&mut file).unwrap();
        }

        if !quiet {
            println!("success");
        }
    }

    Ok(())
}

fn make_opts() -> getopts::Options {
    let mut opts = getopts::Options::new();
    opts.optopt(
        "f",
        "format",
        "The format of the output file. Possible formats: binary, binstr, hexstr, bindump, hexdump",
        "FORMAT",
    );
    opts.optmulti(
        "i",
        "include",
        "Specifies an additional file for processing before the given <asm-files>.",
        "FILE",
    );
    opts.optopt("o", "output", "The name of the output file.", "FILE");
    opts.optflag(
        "p",
        "print",
        "Print output to stdout instead of writing to a file.",
    );
    opts.optopt(
        "",
        "flame",
        "The output flame graph. Can export .json, .html, or text (any other extension)",
        "FLAME",
    );
    opts.optflag("q", "quiet", "Suppress progress reports.");
    opts.optflag("v", "version", "Display version information.");
    opts.optflag("h", "help", "Display this information.");

    opts
}

fn parse_opts(
    report: RcReport,
    opts: &getopts::Options,
    args: &Vec<String>,
) -> Result<getopts::Matches, ()> {
    match opts.parse(&args[1..]) {
        Ok(m) => Ok(m),
        Err(f) => Err(report.error(format!("{}", f))),
    }
}

fn print_usage(opts: &getopts::Options) {
    println!(
        "{}",
        opts.usage(&format!(
            "Usage: {} [options] <asm-file-1> ... <asm-file-N>",
            env!("CARGO_PKG_NAME")
        ))
    );
}

fn print_version() {
    println!("{} v{}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
}

fn print_header() {
    print_version();
}

fn get_default_output_filename(report: RcReport, input_filename: &str) -> Result<String, ()> {
    use std::path::PathBuf;

    let mut output_filename = PathBuf::from(input_filename);
    output_filename.set_extension("bin");

    let output_filename = output_filename
        .to_string_lossy()
        .into_owned()
        .replace("\\", "/");

    if output_filename == input_filename {
        return Err(report.error("cannot derive safe output filename"));
    }

    Ok(output_filename)
}

pub fn assemble(
    report: RcReport,
    fileserver: &FileServer,
    filenames: &[String],
    quiet: bool,
) -> Result<BinaryOutput, ()> {
    if !quiet {
        print_header();
    }

    let mut asm = AssemblerState::new();

    for filename in filenames {
        let filename_owned = filename.clone();

        if !quiet {
            println!("assembling `{}`...", &filename_owned);
        }

        asm.process_file(report.clone(), fileserver, filename_owned)?;
    }

    asm.wrapup(report)?;
    Ok(asm.get_binary_output())
}
