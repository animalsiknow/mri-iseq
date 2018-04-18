use encoding::{self, Encoding};
use lazy_array::LazyArray;
use nom::{self, le_i32, le_i64, le_u32};
use num_traits::FromPrimitive;
use std::{collections, convert, fmt, result, str, borrow::{self, Borrow}};

#[repr(u32)]
pub enum ErrorKind {
  SymbolIsNotAString = 1024,
}

lazy_static! {
  static ref ERROR_NAMES: collections::HashMap<u32, &'static str> = {
    let mut error_names = collections::HashMap::new();
    error_names.insert(
      nom::error_to_u32::<&[u8]>(&nom::ErrorKind::Tag),
      nom::ErrorKind::Tag::<&[u8]>.description(),
    );
    error_names
  };
}

lazy_static! {
  static ref ERROR_PATTERNS: collections::HashMap<Vec<(&'static [u8], nom::ErrorKind)>, &'static str> = {
    let error_patterns = collections::HashMap::new();
    // TODO
    error_patterns
  };
}

#[derive(Debug)]
pub enum Error<'source> {
  Parse(&'source [u8], nom::Err<&'source [u8]>),
}

impl<'source> fmt::Display for Error<'source> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &Error::Parse(_, nom::Err::Incomplete(_)) => write!(f, "error: truncated input"),
      &Error::Parse(_, nom::Err::Failure(_)) => write!(f, "error: internal"),
      &Error::Parse(source, nom::Err::Error(ref context)) => {
        let error = Err(nom::Err::Error(context.clone()));
        if let Some(errors) = nom::prepare_errors::<&[u8], u32>(source, error) {
          writeln!(
            f,
            "================== Error: Invalid IBF =================="
          )?;
          if let Some(message) = ERROR_PATTERNS.get(&nom::error_to_list(context)) {
            writeln!(f, "{}", message)?;
          }
          let colours = nom::generate_colors(&errors);
          write!(
            f,
            "parsers: {}\n{}",
            nom::print_codes(&colours, &ERROR_NAMES),
            nom::print_offsets(source, 0, &errors)
          )
        } else {
          write!(f, "error: oops")
        }
      }
    }
  }
}

pub type Result<'source, T> = result::Result<T, Error<'source>>;

named!(
  cstr<&str>,
  map_res!(take_until_and_consume!("\0"), |bytes| str::from_utf8(bytes))
);

named!(
  size_from_unsigned_int<usize>,
  do_parse!(raw: le_u32 >> (raw as usize))
);

named!(
  size_from_long<usize>,
  do_parse!(raw: le_i64 >> (raw as usize))
);

named!(magic, tag!(b"YARB"));

named!(
  version<Version>,
  switch!(
    do_parse!(
      major: le_i32 >>
      minor: le_i32 >>
      (major, minor)
    ),
    (2, 5) => value!(Version::Ibf25)
  )
);

named!(
  header<Header>,
  do_parse!(
    magic >> version: version >> size: size_from_unsigned_int >> extra_size: size_from_unsigned_int
      >> iseq_list_size: size_from_unsigned_int >> id_list_size: size_from_unsigned_int
      >> object_list_size: size_from_unsigned_int >> iseq_list_offset: size_from_unsigned_int
      >> id_list_offset: size_from_unsigned_int >> object_list_offset: size_from_unsigned_int
      >> platform: cstr >> (Header {
      version,
      size,
      extra_size,
      iseq_list_size,
      id_list_size,
      object_list_size,
      iseq_list_offset,
      id_list_offset,
      object_list_offset,
      platform,
    })
  )
);

pub fn parse_header(ibf_source: &[u8]) -> Result<Header> {
  match header(ibf_source) {
    Ok((_rest, header)) => Ok(header),
    Err(err) => Err(Error::Parse(ibf_source, err)),
  }
}

named!(
  object_header<RawValueHeader>,
  do_parse!(raw: le_u32 >> (RawValueHeader(raw)))
);

fn convert_string(encoding_index: i64, data: &[u8]) -> Value {
  Value::String(match StringEncoding::from_i64(encoding_index) {
    Some(StringEncoding::Utf8) | Some(StringEncoding::UsAscii) => {
      str::from_utf8(data).unwrap().into()
    }
    Some(StringEncoding::Ascii) => encoding::all::ISO_8859_1
      .decode(data, encoding::DecoderTrap::Strict)
      .unwrap()
      .into(),
    encoding => panic!("Unsupported encoding {:?}.", encoding),
  })
}

named!(
  string<Value>,
  do_parse!(
    encoding_index: le_i64 >> length: le_i64 >> data: take!(length)
      >> (convert_string(encoding_index, data))
  )
);

fn array<'loader, 'source: 'loader>(
  object_source: &'source [u8],
  object_loader: &'loader Loader<'loader, 'source>,
) -> nom::IResult<&'source [u8], Value<'loader, 'source>> {
  let (mut rest, length) = size_from_long(object_source)?;
  let mut elements = Vec::with_capacity(length);
  for _ in 0..length {
    let (next, index) = size_from_long(rest)?;
    rest = next;
    // TODO
    elements.push(object_loader.load_object(index).unwrap());
  }
  Ok((rest, Value::Array(elements)))
}

fn symbol<'loader, 'source: 'loader>(
  object_source: &'source [u8],
  object_loader: &'loader Loader<'loader, 'source>,
) -> nom::IResult<&'source [u8], Value<'loader, 'source>> {
  let (rest, index) = size_from_long(object_source)?;
  match object_loader.load_object(index) {
    Ok(&Value::String(ref s)) => Ok((rest, Value::Symbol(s.borrow()))),
    Ok(_) => Err(nom::Err::Error(nom::Context::Code(
      object_source,
      nom::ErrorKind::Custom(ErrorKind::SymbolIsNotAString as u32),
    ))),
    Err(err) => panic!("TODO: {:?}", err),
  }
}

named!(
  fixnum<Value>,
  do_parse!(tagged_value: le_i64 >> (Value::Fixnum(tagged_value >> 1)))
);

fn object<'loader, 'source: 'loader>(
  object_source: &'source [u8],
  object_loader: &'loader Loader<'loader, 'source>,
) -> nom::IResult<&'source [u8], Value<'loader, 'source>> {
  let (rest, header) = object_header(object_source)?;
  match header.ty() {
    ValueTy::String => string(rest),
    ValueTy::Array => array(rest, object_loader),
    ValueTy::Nil => Ok((rest, Value::Nil)),
    ValueTy::True => Ok((rest, Value::True)),
    ValueTy::False => Ok((rest, Value::False)),
    ValueTy::Symbol => symbol(rest, object_loader),
    ValueTy::Fixnum => fixnum(rest),
    _ => Ok((
      rest,
      Value::String(format!("Unknown type {:?}", header).into()),
    )),
  }
}

named_args!(
  offset_list<'a>(offset: usize, size: usize) <Vec<usize>>,
  preceded!(take!(offset), count!(size_from_unsigned_int, size))
);

named_args!(
  index_list(offset: usize, size: usize) <Vec<usize>>,
  preceded!(take!(offset), count!(size_from_long, size))
);

pub struct Loader<'loader, 'source: 'loader> {
  objects: LazyArray<Value<'loader, 'source>>,
  ids: LazyArray<Id<'loader>>,
  object_offsets: Vec<usize>,
  id_indices: Vec<usize>,
  source: &'source [u8],
}

impl<'loader, 'source: 'loader> Loader<'loader, 'source> {
  pub fn new(
    header: &Header<'source>,
    source: &'source [u8],
  ) -> Result<'source, Loader<'loader, 'source>> {
    let object_offsets =
      match offset_list(source, header.object_list_offset, header.object_list_size) {
        Ok((_rest, offsets)) => offsets,
        Err(err) => return Err(Error::Parse(source, err)),
      };
    let id_indices = match index_list(source, header.id_list_offset, header.id_list_size) {
      Ok((_rest, indices)) => indices,
      Err(err) => return Err(Error::Parse(source, err)),
    };
    Ok(Loader {
      objects: LazyArray::new(header.object_list_size),
      ids: LazyArray::new(header.id_list_size),
      object_offsets: object_offsets,
      id_indices: id_indices,
      source: source,
    })
  }

  pub fn id_count(&self) -> usize {
    self.id_indices.len()
  }

  pub fn object_count(&self) -> usize {
    self.object_offsets.len()
  }

  pub fn load_id(&'loader self, index: usize) -> Result<'source, &'loader Id<'loader>> {
    match self.ids.get(index) {
      Some(value) => Ok(value),
      None => {
        let id = self.do_load_id(index)?;
        Ok(self.ids.get_or_insert(index, id))
      }
    }
  }

  pub fn load_object(
    &'loader self,
    index: usize,
  ) -> Result<'source, &'loader Value<'loader, 'source>> {
    match self.objects.get(index) {
      Some(value) => Ok(value),
      None => {
        let object = self.do_load_object(index)?;
        Ok(self.objects.get_or_insert(index, object))
      }
    }
  }

  fn do_load_id(&'loader self, index: usize) -> Result<'source, Id<'loader>> {
    let object_index = self.id_indices[index];
    match self.load_object(object_index)? {
      &Value::String(ref s) => Ok(Id(s.borrow())),
      &Value::Nil => Ok(Id("")),
      _ => panic!("TODO"),
    }
  }

  fn do_load_object(&'loader self, index: usize) -> Result<'source, Value<'loader, 'source>> {
    let object_source = &self.source[self.object_offsets[index]..];
    match object(object_source, self) {
      Ok((_rest, object)) => Ok(object),
      Err(err) => Err(Error::Parse(object_source, err)),
    }
  }
}

#[derive(Debug)]
pub enum Version {
  Ibf25,
}

#[derive(Debug)]
pub struct Document {
  pub version: Version,
}

#[derive(Debug)]
pub struct Header<'source> {
  pub version: Version,
  pub size: usize,
  pub extra_size: usize,
  pub iseq_list_size: usize,
  pub id_list_size: usize,
  pub object_list_size: usize,
  pub iseq_list_offset: usize,
  pub id_list_offset: usize,
  pub object_list_offset: usize,
  pub platform: &'source str,
}

#[derive(Debug)]
pub enum IseqType {
  Top,
  Method,
  Block,
  Class,
  Rescue,
  Ensure,
  Eval,
  Main,
  DefinedGuard,
}

#[derive(Clone, Debug)]
pub struct Id<'loader>(&'loader str);

#[derive(Clone, Debug)]
pub enum Value<'loader, 'source: 'loader> {
  String(borrow::Cow<'source, str>),
  Array(Vec<&'loader Value<'loader, 'source>>),
  Nil,
  True,
  False,
  Symbol(&'loader str),
  Fixnum(i64),
}

bitfield!{
  pub struct RawValueHeader(u32);
  impl Debug;
  pub u8, into ValueTy, ty, _: 4, 0;
  pub special_const, _: 5;
  pub frozen, _: 6;
  pub internal, _: 7;
}

#[derive(Debug, FromPrimitive)]
pub enum ValueTy {
  None = 0x00,
  Object = 0x01,
  Class = 0x02,
  Module = 0x03,
  Float = 0x04,
  String = 0x05,
  Regexp = 0x06,
  Array = 0x07,
  Hash = 0x08,
  Struct = 0x09,
  Bignum = 0x0a,
  File = 0x0b,
  Data = 0x0c,
  Match = 0x0d,
  Complex = 0x0e,
  Rational = 0x0f,
  Unknown10 = 0x10,
  Nil = 0x11,
  True = 0x12,
  False = 0x13,
  Symbol = 0x14,
  Fixnum = 0x15,
  Undef = 0x16,
  Unknown17 = 0x17,
  Unknown18 = 0x18,
  Unknown19 = 0x19,
  Imemo = 0x1a,
  Node = 0x1b,
  Iclass = 0x1c,
  Zombie = 0x1d,
  Unknown1e = 0x1e,
  Unknown1f = 0x1f,
}

impl convert::From<u8> for ValueTy {
  fn from(raw: u8) -> ValueTy {
    ValueTy::from_u8(raw).unwrap_or(ValueTy::None)
  }
}

#[derive(Debug, FromPrimitive)]
pub enum StringEncoding {
  Ascii = 0x0,
  Utf8 = 0x1,
  UsAscii = 0x2,
  Utf16Be = 0x3,
  Utf16Le = 0x4,
  Utf32Be = 0x5,
  Utf32Le = 0x6,
  Utf16 = 0x7,
  Utf32 = 0x8,
  Utf8Mac = 0x9,
  EucJp = 0xa,
  Windows31J = 0xb,
}

#[derive(Debug)]
pub struct KwParam<'loader, 'source: 'loader> {
  pub name: &'source str,
  pub default_value: Option<Value<'loader, 'source>>,
}

#[derive(Debug)]
pub struct IseqParamSpec<'loader, 'source: 'loader> {
  pub size_lead: u32,
  pub size_opt: u32,
  pub has_rest: bool,
  pub size_post: u32,
  pub keywords: Vec<KwParam<'loader, 'source>>,
  pub has_kw_rest: bool,
  pub has_block: bool,
}

#[derive(Debug)]
pub struct Iseq<'loader, 'source: 'loader> {
  pub ty: IseqType,
  pub param_spec: IseqParamSpec<'loader, 'source>,
}
