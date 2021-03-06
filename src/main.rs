extern crate clap;
extern crate mri_iseq;
extern crate nom;
extern crate subprocess;

use mri_iseq::ibf;
use std::io::Read;
use std::{fs, io, path, process, result};

const DISASSEMBLE_HELP: &'static str = "Disassemble MRI instruction sequences.";
const DISASSEMBLE_INPUT_HELP: &'static str =
  &"File to disassemble. It can either be an instruction sequence dump or a Ruby file.";

const COMPILE_HELP: &'static str = "";
const COMPILE_INPUT_HELP: &'static str = &"";
const COMPILE_OUTPUT_HELP: &'static str = &"";

#[derive(Debug)]
enum Error {
  Parse(String),
  Io(io::Error),
  Popen(subprocess::PopenError),
  Cli(clap::Error),
  Ruby,
}

impl<'source> From<ibf::Error<'source>> for Error {
  fn from(err: ibf::Error<'source>) -> Self {
    Error::Parse(format!("{}", err))
  }
}

impl From<io::Error> for Error {
  fn from(err: io::Error) -> Self {
    Error::Io(err)
  }
}

impl From<subprocess::PopenError> for Error {
  fn from(err: subprocess::PopenError) -> Self {
    Error::Popen(err)
  }
}

impl From<clap::Error> for Error {
  fn from(err: clap::Error) -> Self {
    Error::Cli(err)
  }
}

type Result<T> = result::Result<T, Error>;

fn app<'a, 'b>() -> clap::App<'a, 'b> {
  clap::App::new("mri-iseq")
    .version("1.0")
    .about("Tool to manipulate MRI instruction sequences.")
    .author("Simon Génier")
    .subcommand(
      clap::SubCommand::with_name("disassemble")
        .visible_alias("d")
        .about(DISASSEMBLE_HELP)
        .arg(
          clap::Arg::with_name("INPUT")
            .help(DISASSEMBLE_INPUT_HELP)
            .required(true)
            .index(1),
        ),
    )
    .subcommand(
      clap::SubCommand::with_name("compile")
        .visible_alias("c")
        .about(COMPILE_HELP)
        .arg(
          clap::Arg::with_name("INPUT")
            .help(COMPILE_INPUT_HELP)
            .required(true)
            .index(1),
        )
        .arg(
          clap::Arg::with_name("OUTPUT")
            .help(COMPILE_OUTPUT_HELP)
            .required(true)
            .index(2),
        ),
    )
}

fn print_ibf_header<'source>(header: &ibf::Header) {
  println!("IBF Header:");
  println!("  Magic:\t\t\tYARB");
  println!(
    "  Version:\t\t\t{}",
    match header.version {
      ibf::Version::Ibf25 => "2.5",
    }
  );
  println!("  Size:\t\t\t\t{}", header.size);
  println!("  Extra size:\t\t\t{}", header.extra_size);
  println!(
    "  Instruction sequences:\t{} entries at offset Ox{:x}",
    header.iseq_list_size, header.iseq_list_offset
  );
  println!(
    "  Ids:\t\t\t\t{} entries at offset Ox{:x}",
    header.id_list_size, header.id_list_offset
  );
  println!(
    "  Objects:\t\t\t{} entries at offset Ox{:x}",
    header.object_list_size, header.object_list_offset
  );
  println!("  Platform:\t\t\t{}", header.platform);
}

fn print_ibf_iseqs<'loader, 'source: 'loader>(
  loader: &'loader ibf::Loader<'loader, 'source>,
) -> Result<()> {
  println!("Iseqs:");
  for i in 0..loader.iseq_count() {
    println!("  {}: {:?}", i, loader.load_iseq(i)?);
  }
  Ok(())
}

fn print_ibf_ids<'source, 'loader: 'source>(
  loader: &'loader ibf::Loader<'source, 'loader>,
) -> Result<()> {
  println!("Ids:");
  for i in 0..loader.id_count() {
    println!("  {}: {:?}", i, loader.load_id(i)?);
  }
  Ok(())
}

fn print_ibf_objects<'source, 'loader: 'source>(
  loader: &'loader ibf::Loader<'source, 'loader>,
) -> Result<()> {
  println!("Objects:");
  for i in 0..loader.object_count() {
    println!("  {}: {:?}", i, loader.load_object(i)?);
  }
  Ok(())
}

fn disassemble_ibf(ibf_source: &[u8]) -> Result<()> {
  let header = ibf::parse_header(ibf_source)?;
  print_ibf_header(&header);
  let loader = ibf::Loader::new(&header, ibf_source)?;
  println!("");
  print_ibf_iseqs(&loader)?;
  println!("");
  print_ibf_ids(&loader)?;
  println!("");
  print_ibf_objects(&loader)?;
  Ok(())
}

const DUMP_ISEQ_PROGRAM_SOURCE: &'static str =
  "print(RubyVM::InstructionSequence.compile(STDIN.read).to_binary)";

fn disassemble_rb(file_path: &path::Path) -> Result<()> {
  let file = fs::File::open(file_path)?;
  let mut ibf_source_stream = subprocess::Exec::cmd("ruby")
    .arg("-e")
    .arg(DUMP_ISEQ_PROGRAM_SOURCE)
    .stdin(file)
    .stdout(subprocess::Redirection::Pipe)
    .stream_stdout()?;
  let mut ibf_source = vec![];
  ibf_source_stream.read_to_end(&mut ibf_source)?;
  disassemble_ibf(&mut ibf_source)
}

fn disassemble(file_name: &str) -> Result<()> {
  let file_path = path::Path::new(file_name);
  if file_path.extension().and_then(|ext| ext.to_str()) == Some("rb") {
    disassemble_rb(&file_path)
  } else {
    let mut ibf_source_stream = fs::File::open(file_path)?;
    let mut ibf_source = vec![];
    ibf_source_stream.read_to_end(&mut ibf_source)?;
    disassemble_ibf(&mut ibf_source)
  }
}

fn compile(input_path_name: &str, output_path_name: &str) -> Result<()> {
  let input_path = path::Path::new(input_path_name);
  let input_file = fs::File::open(input_path)?;
  let output_path = path::Path::new(output_path_name);
  let output_file = fs::File::create(output_path)?;
  let status = subprocess::Exec::cmd("ruby")
    .arg("-e")
    .arg(DUMP_ISEQ_PROGRAM_SOURCE)
    .stdin(input_file)
    .stdout(output_file)
    .join()?;
  if status.success() {
    Ok(())
  } else {
    Err(Error::Ruby)
  }
}

fn print_help() -> Result<()> {
  app().print_help()?;
  Ok(())
}

fn go() -> Result<()> {
  let matches = app().get_matches();

  if let Some(disassemble_matches) = matches.subcommand_matches("disassemble") {
    match disassemble_matches.value_of("INPUT") {
      Some(file_name) => disassemble(file_name),
      None => print_help(),
    }
  } else if let Some(compile_matches) = matches.subcommand_matches("compile") {
    match (
      compile_matches.value_of("INPUT"),
      compile_matches.value_of("OUTPUT"),
    ) {
      (Some(input_path_name), Some(output_path_name)) => compile(input_path_name, output_path_name),
      _ => print_help(),
    }
  } else {
    print_help()
  }
}

fn main() {
  process::exit(match go() {
    Ok(()) => 0,
    Err(ref err) => {
      match err {
        &Error::Parse(ref message) => {
          println!("{}", message);
        }
        &Error::Ruby => {
          println!("The Ruby process did not exit successfuly, see its output for more details.");
        }
        err => {
          println!("{:?}", err);
        }
      };
      1
    }
  })
}
