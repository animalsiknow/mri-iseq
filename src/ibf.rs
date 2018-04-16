use encoding::{self, Encoding};
use lazy_array::LazyArray;
use nom::{self, le_i32, le_i64, le_u32, le_u64};
use num_traits::FromPrimitive;
use std::{borrow, collections, convert, fmt, result, str};

pub const BAD_MAGIC: u32 = 1024;
pub const UNSUPPORTED_VERSION: u32 = 1025;

#[derive(Debug)]
pub enum Error {
  Parse(String),
}

pub type Result<T> = result::Result<T, Error>;

named!(
  cstr<&str>,
  map_res!(take_until_and_consume!("\0"), |bytes| str::from_utf8(bytes))
);

named!(
  magic,
  add_return_error!(ErrorKind::Custom(BAD_MAGIC), tag!(b"YARB"))
);

named!(
  version<Version>,
  add_return_error!(
    ErrorKind::Custom(UNSUPPORTED_VERSION),
    switch!(
      do_parse!(
        major: le_i32 >>
        minor: le_i32 >>
        (major, minor)
      ),
      (2, 5) => value!(Version::Ibf25)
    )
  )
);

named!(
  pub header<RawHeader>,
  do_parse!(
    magic >>
    version: version >>
    size: le_i32 >>
    extra_size: le_i32 >>
    iseq_list_size: le_i32 >>
    id_list_size: le_i32 >>
    object_list_size: le_i32 >>
    iseq_list_offset: le_i32 >>
    id_list_offset: le_i32 >>
    object_list_offset: le_i32 >>
    platform: cstr >>
    (
      RawHeader {
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
      }
    )
  )
);

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

named_args!(
  array<'a>(object_loader: &'a ObjectLoader<'a, 'a>) <&'a [u8], Value<'a, 'a>>,
  do_parse!(
    length: le_u64 >>
    elements: count!(
      do_parse!(
        index: le_i64 >> (object_loader.load(index as usize).unwrap())
      ),
      length as usize
    ) >>
    (Value::Array(elements))
  )
);

named!(
  fixnum<Value>,
  do_parse!(tagged_value: le_i64 >> (Value::Fixnum(tagged_value >> 1)))
);

named_args!(
  pub object<'a>(object_loader: &'a ObjectLoader<'a, 'a>) <&'a [u8], Value<'a, 'a>>,
  do_parse!(
    header: object_header >>
    value: switch!(
      value!(header.ty()),
      ValueTy::String => call!(string) |
      ValueTy::Array => apply!(array, object_loader) |
      ValueTy::Nil => value!(Value::Nil) |
      ValueTy::Fixnum => call!(fixnum) |
      _ => value!(Value::String(format!("{:?}", header).into()))
    ) >>
    (value)
  )
);

named_args!(
  offsets(object_count: usize) <Vec<usize>>,
  count!(
    do_parse!(offset: le_i32 >> (offset as usize)),
    object_count
  )
);

pub struct ObjectLoader<'objects, 'source: 'objects> {
  objects: LazyArray<Value<'objects, 'source>>,
  offsets: Vec<usize>,
  source: &'source [u8],
}

impl<'objects, 'source: 'objects> ObjectLoader<'objects, 'source> {
  pub fn new(header: &RawHeader<'source>, source: &'source [u8]) -> Self {
    let object_len = header.object_list_size as usize;
    let (_rest, offsets) =
      offsets(&source[header.object_list_offset as usize..], object_len).unwrap();
    ObjectLoader {
      objects: LazyArray::new(object_len),
      offsets: offsets,
      source: source,
    }
  }

  pub fn len(&self) -> usize {
    self.offsets.len()
  }

  pub fn load(&'objects self, index: usize) -> Result<&'objects Value<'source, 'objects>> {
    match self.objects.get(index) {
      Some(ref value) => Ok(value),
      None => {
        let object = self.do_load(index)?;
        Ok(self.objects.get_or_insert(index, object))
      }
    }
  }

  fn do_load(&'objects self, index: usize) -> Result<Value<'source, 'objects>> {
    let object_source = &self.source[self.offsets[index]..];
    match object(object_source, self) {
      Ok((_rest, object)) => Ok(object),
      err => Err(Error::Parse(display_error(&object_source, err))),
    }
  }
}

fn generate_colours(v: &[(nom::ErrorKind<u32>, usize, usize)]) -> collections::HashMap<u32, u8> {
  let mut h: collections::HashMap<u32, u8> = collections::HashMap::new();
  let mut color = 0;

  for &(ref c, _, _) in v.iter() {
    let code = match c {
      &nom::ErrorKind::Custom(code) => code,
      c => nom::error_to_u32(c),
    };
    h.insert(code, color + 31);
    color = color + 1 % 7;
  }

  h
}

// TODO remove pub
pub fn display_error<O: fmt::Debug>(source: &[u8], result: nom::IResult<&[u8], O>) -> String {
  let mut h: collections::HashMap<u32, &str> = collections::HashMap::new();
  h.insert(BAD_MAGIC, "magic");
  h.insert(UNSUPPORTED_VERSION, "version");

  if let Some(errors) = nom::prepare_errors(source, result) {
    let colours = generate_colours(&errors);
    format!(
      "parsers: {}\n{}",
      nom::print_codes(&colours, &h),
      nom::print_offsets(source, 0, &errors)
    )
  } else {
    "?".into()
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
pub struct RawHeader<'source> {
  pub version: Version,
  pub size: i32,
  pub extra_size: i32,
  pub iseq_list_size: i32,
  pub id_list_size: i32,
  pub object_list_size: i32,
  pub iseq_list_offset: i32,
  pub id_list_offset: i32,
  pub object_list_offset: i32,
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
pub enum Value<'objects, 'source: 'objects> {
  String(borrow::Cow<'source, str>),
  Array(Vec<&'objects Value<'objects, 'source>>),
  Nil,
  True,
  False,
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
pub struct KwParam<'objects, 'source: 'objects> {
  pub name: &'source str,
  pub default_value: Option<Value<'objects, 'source>>,
}

#[derive(Debug)]
pub struct IseqParamSpec<'objects, 'source: 'objects> {
  pub size_lead: u32,
  pub size_opt: u32,
  pub has_rest: bool,
  pub size_post: u32,
  pub keywords: Vec<KwParam<'objects, 'source>>,
  pub has_kw_rest: bool,
  pub has_block: bool,
}

#[derive(Debug)]
pub struct Iseq<'objects, 'source: 'objects> {
  pub ty: IseqType,
  pub param_spec: IseqParamSpec<'objects, 'source>,
}
