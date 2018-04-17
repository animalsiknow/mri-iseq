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

#[derive(Debug)]
enum Error {
  Parse(String),
  Io(io::Error),
  Popen(subprocess::PopenError),
  Cli(clap::Error),
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
}

fn print_ibf_header<'source>(header: &ibf::RawHeader) {
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

fn print_ibf_objects<'source>(
  header: &'source ibf::RawHeader,
  ibf_source: &'source [u8],
) -> Result<()> {
  println!("Objects:");
  let object_store = ibf::ObjectLoader::new(header, ibf_source);
  for i in 0..object_store.len() {
    println!("  {}: {:?}", i, object_store.load(i)?);
  }
  Ok(())
}

fn disassemble_ibf(ibf_source: &[u8]) -> Result<()> {
  let header = ibf::parse_header(ibf_source)?;
  print_ibf_header(&header);
  println!("");
  print_ibf_objects(&header, ibf_source)
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
        err => {
          println!("{:?}", err);
        }
      };
      1
    }
  })
}
