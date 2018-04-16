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
  Ibf(ibf::Error),
  Io(io::Error),
  Popen(subprocess::PopenError),
  Cli(clap::Error),
}

impl From<ibf::Error> for Error {
  fn from(err: ibf::Error) -> Self {
    Error::Ibf(err)
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

fn disassemble_ibf_header(ibf_source: &[u8]) -> ibf::Result<ibf::RawHeader> {
  match ibf::header(&ibf_source) {
    Ok((_rest, header)) => Ok(header),
    result => Err(ibf::Error::Parse(ibf::display_error(&ibf_source, result))),
  }
}

fn print_ibf_objects<'source>(
  header: &ibf::RawHeader,
  ibf_source: &'source [u8],
) -> ibf::Result<()> {
  let object_store = ibf::ObjectLoader::new(header, ibf_source);
  for i in 0..object_store.len() {
    println!("{}: {:?}", i, object_store.load(i)?);
  }
  Ok(())
}

fn disassemble_ibf(ibf_source: &[u8]) -> ibf::Result<()> {
  let header = disassemble_ibf_header(ibf_source)?;
  println!("{:?}", header);
  print_ibf_objects(&header, ibf_source)?;
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
  disassemble_ibf(&mut ibf_source)?;
  Ok(())
}

fn disassemble(file_name: &str) -> Result<()> {
  let file_path = path::Path::new(file_name);
  if file_path.extension().and_then(|ext| ext.to_str()) == Some("rb") {
    disassemble_rb(&file_path)
  } else {
    let mut ibf_source_stream = fs::File::open(file_path)?;
    let mut ibf_source = vec![];
    ibf_source_stream.read_to_end(&mut ibf_source)?;
    disassemble_ibf(&mut ibf_source)?;
    Ok(())
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
    Err(Error::Ibf(ibf::Error::Parse(message))) => {
      println!("{}", message);
      1
    }
    Err(err) => {
      println!("{:?}", err);
      1
    }
  })
}
