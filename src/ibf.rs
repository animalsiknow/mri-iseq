use bitfield::bitfield;
use cursed_collections::LazyArray;
use encoding::{self, Encoding};
use lazy_static::lazy_static;
use nom::{self, *};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::{
  borrow::{self, Borrow},
  collections, convert, fmt, result, str,
};

#[repr(u32)]
pub enum ErrorKind {
  ObjectIsNotAString = 1024,
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
          writeln!(f, "{:=^80}", " Error: Invalid IBF ")?;
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

impl<'source> From<Error<'source>> for nom::Err<&'source [u8]> {
  fn from(err: Error<'source>) -> Self {
    match err {
      Error::Parse(_, inner_err) => inner_err,
    }
  }
}

pub type Result<'source, T> = result::Result<T, Error<'source>>;

named!(
  cstr<&str>,
  map_res!(take_until_and_consume!("\0"), |bytes| str::from_utf8(bytes))
);

named!(u16_from_ptr<u16>, do_parse!(raw: le_i64 >> (raw as u16)));

named!(i32_from_signed_int<i32>, do_parse!(raw: le_i32 >> (raw)));

named!(
  size_from_unsigned_int<usize>,
  do_parse!(raw: le_u32 >> (raw as usize))
);

named!(
  size_from_signed_int<usize>,
  do_parse!(raw: le_i32 >> (raw as usize))
);

named!(
  size_from_long<usize>,
  do_parse!(raw: le_i64 >> (raw as usize))
);

named!(
  size_from_ptr<usize>,
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
    magic
      >> version: version
      >> size: size_from_unsigned_int
      >> extra_size: size_from_unsigned_int
      >> iseq_list_size: size_from_unsigned_int
      >> id_list_size: size_from_unsigned_int
      >> object_list_size: size_from_unsigned_int
      >> iseq_list_offset: size_from_unsigned_int
      >> id_list_offset: size_from_unsigned_int
      >> object_list_offset: size_from_unsigned_int
      >> platform: cstr
      >> (Header {
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
    encoding_index: le_i64
      >> length: le_i64
      >> data: take!(length)
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
  Ok((rest, Value::Symbol(object_loader.load_string(index)?)))
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

named!(
  iseq_type<IseqType>,
  map_opt!(le_i32, FromPrimitive::from_i32)
);

named!(
  iseq_param_flags<IseqParamFlags>,
  do_parse!(raw: le_u32 >> (IseqParamFlags(raw)))
);

named!(
  iseq_param_spec<IseqParamSpec>,
  do_parse!(
    flags: iseq_param_flags
      >> _size: size_from_unsigned_int
      >> lead_size: size_from_signed_int
      >> optional_size: size_from_signed_int
      >> _rest_start: size_from_signed_int
      >> _post_start: size_from_signed_int
      >> post_size: size_from_signed_int
      >> _block_start: size_from_signed_int
      >> _opt_table: size_from_ptr
      >> _keywords: size_from_ptr
      >> (IseqParamSpec {
        size_lead: lead_size,
        size_opt: optional_size,
        has_rest: flags.has_rest(),
        size_post: post_size,
        keywords: vec![],
        has_kw_rest: flags.has_kwrest(),
        has_block: flags.has_block(),
      })
  )
);

fn make_source_path_from_array<'loader>(a: &Vec<&'loader Value>) -> SourcePath<'loader> {
  match (a[0], a[1]) {
    (&Value::String(ref path), &Value::Nil) => SourcePath::Path(path.borrow()),
    (&Value::String(ref path), &Value::String(ref real_path)) => SourcePath::RealPath {
      path: path.borrow(),
      real_path: real_path.borrow(),
    },
    _ => panic!("TODO"),
  }
}

fn source_path<'loader, 'source: 'loader>(
  source: &'source [u8],
  object_loader: &'loader Loader<'loader, 'source>,
) -> nom::IResult<&'source [u8], SourcePath<'loader>> {
  let (next_source, index) = size_from_ptr(source)?;
  match object_loader.load_object(index)? {
    &Value::String(ref s) => Ok((next_source, SourcePath::Path(s.borrow()))),
    &Value::Array(ref a) => Ok((next_source, make_source_path_from_array(a))),
    _ => Err(nom::Err::Error(nom::Context::Code(
      source,
      nom::ErrorKind::Custom(ErrorKind::ObjectIsNotAString as u32),
    ))),
  }
}

named!(
  source_location<SourceLocation>,
  do_parse!(
    line: i32_from_signed_int >> column: i32_from_signed_int >> (SourceLocation { line, column })
  )
);

named!(
  source_range<SourceRange>,
  do_parse!(
    first_location: source_location
      >> last_location: source_location
      >> (SourceRange {
        first_location,
        last_location
      })
  )
);

fn iseq_location<'loader, 'source: 'loader>(
  source: &'source [u8],
  object_loader: &'loader Loader<'loader, 'source>,
) -> nom::IResult<&'source [u8], IseqLocation<'loader>> {
  let (source, path) = source_path(source, object_loader)?;

  let (source, index) = size_from_ptr(source)?;
  let base_label = object_loader.load_string(index)?;

  let (source, index) = size_from_ptr(source)?;
  let label = object_loader.load_string(index)?;

  let (source, first_line_number) = u16_from_ptr(source)?;

  let (source, source_range) = source_range(source)?;

  Ok((
    source,
    IseqLocation {
      path,
      base_label,
      label,
      first_line_number,
      source_range,
    },
  ))
}

fn iseq<'loader, 'source: 'loader>(
  source: &'source [u8],
  object_loader: &'loader Loader<'loader, 'source>,
) -> nom::IResult<&'source [u8], Iseq<'loader, 'source>> {
  // Offset: 0x00
  let (source, ty) = iseq_type(source)?;
  // Offset: 0x04
  let (source, size) = size_from_unsigned_int(source)?;
  // Offset: 0x08
  let (source, _iseq_encoded) = size_from_ptr(source)?;
  // Offset: 0x10
  let (source, param_spec) = iseq_param_spec(source)?;
  // Offset: 0x40
  let (source, location) = iseq_location(source, object_loader)?;
  // Offset: 0x70
  let (source, _insns_info) = size_from_ptr(source)?;
  // Offset: 0x78
  let (source, _local_table) = size_from_ptr(source)?;
  // Offset: 0x80
  let (source, _catch_table) = size_from_ptr(source)?;
  // Offset: 0x88
  let (source, _parent_iseq) = size_from_ptr(source)?;
  // Offset: 0x90
  let (source, _local_iseq) = size_from_ptr(source)?;
  // Offset: 0x98
  let (source, _is_entries) = size_from_ptr(source)?;
  // Offset: 0xa0
  let (source, _ci_entries) = size_from_ptr(source)?;
  // Offset: 0xa8
  let (source, _cc_entries) = size_from_ptr(source)?;
  // Offset: 0xb0
  let (source, _mark_array) = size_from_ptr(source)?;
  // Offset: 0xb8
  let (source, _local_table_size) = size_from_unsigned_int(source)?;
  // Offset: 0xbc
  let (source, _is_size) = size_from_unsigned_int(source)?;
  // Offset: 0xc0
  let (source, _ci_size) = size_from_unsigned_int(source)?;
  // Offset: 0xc4
  let (source, _ci_kw_size) = size_from_unsigned_int(source)?;
  // Offset: 0xc8
  let (source, _insns_info_size) = size_from_unsigned_int(source)?;
  // Offset: 0xcc
  let (source, _stack_max) = size_from_unsigned_int(source)?;
  // Total size: 0xd0
  Ok((
    source,
    Iseq {
      ty,
      size,
      _iseq_encoded,
      param_spec,
      location,
      _insns_info,
      _local_table,
      _catch_table,
      _parent_iseq,
      _local_iseq,
      _is_entries,
      _ci_entries,
      _cc_entries,
      _mark_array,
      _local_table_size,
      _is_size,
      _ci_size,
      _ci_kw_size,
      _insns_info_size,
      _stack_max,
    },
  ))
}

pub struct Loader<'loader, 'source: 'loader> {
  iseqs: LazyArray<Iseq<'loader, 'source>>,
  ids: LazyArray<Id<'loader>>,
  objects: LazyArray<Value<'loader, 'source>>,
  iseq_offsets: Vec<usize>,
  id_indices: Vec<usize>,
  object_offsets: Vec<usize>,
  source: &'source [u8],
}

impl<'loader, 'source: 'loader> Loader<'loader, 'source> {
  pub fn new(
    header: &Header<'source>,
    source: &'source [u8],
  ) -> Result<'source, Loader<'loader, 'source>> {
    let (_rest, iseq_offsets) = offset_list(source, header.iseq_list_offset, header.iseq_list_size)
      .map_err(|err| Error::Parse(source, err))?;
    let (_rest, id_indices) = index_list(source, header.id_list_offset, header.id_list_size)
      .map_err(|err| Error::Parse(source, err))?;
    let (_rest, object_offsets) =
      offset_list(source, header.object_list_offset, header.object_list_size)
        .map_err(|err| Error::Parse(source, err))?;
    Ok(Loader {
      iseqs: LazyArray::new(header.iseq_list_size),
      ids: LazyArray::new(header.id_list_size),
      objects: LazyArray::new(header.object_list_size),
      iseq_offsets,
      id_indices,
      object_offsets,
      source,
    })
  }

  pub fn iseq_count(&self) -> usize {
    self.iseq_offsets.len()
  }

  pub fn id_count(&self) -> usize {
    self.id_indices.len()
  }

  pub fn object_count(&self) -> usize {
    self.object_offsets.len()
  }

  pub fn load_iseq(
    &'loader self,
    index: usize,
  ) -> Result<'source, &'loader Iseq<'loader, 'source>> {
    self.iseqs.get(index).map_or_else(
      || Ok(self.iseqs.get_or_insert(index, self.do_load_iseq(index)?)),
      |iseq| Ok(iseq),
    )
  }

  pub fn load_id(&'loader self, index: usize) -> Result<'source, &'loader Id<'loader>> {
    self.ids.get(index).map_or_else(
      || Ok(self.ids.get_or_insert(index, self.do_load_id(index)?)),
      |id| Ok(id),
    )
  }

  pub fn load_object(
    &'loader self,
    index: usize,
  ) -> Result<'source, &'loader Value<'loader, 'source>> {
    self.objects.get(index).map_or_else(
      || {
        Ok(
          self
            .objects
            .get_or_insert(index, self.do_load_object(index)?),
        )
      },
      |object| Ok(object),
    )
  }

  pub fn load_string(&'loader self, index: usize) -> Result<'source, &'loader str> {
    match self.load_object(index)? {
      &Value::String(ref s) => Ok(s.borrow()),
      _ => {
        return Err(Error::Parse(
          self.source,
          nom::Err::Error(nom::Context::Code(
            self.source,
            nom::ErrorKind::Custom(ErrorKind::ObjectIsNotAString as u32),
          )),
        ));
      }
    }
  }

  fn do_load_iseq(&'loader self, index: usize) -> Result<'source, Iseq<'loader, 'source>> {
    let iseq_source = &self.source[self.iseq_offsets[index]..];
    match iseq(iseq_source, self) {
      Ok((_rest, iseq)) => Ok(iseq),
      Err(err) => Err(Error::Parse(iseq_source, err)),
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

bitfield! {
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

/// TODO
#[derive(Debug)]
pub enum SourcePath<'loader> {
  Path(&'loader str),
  RealPath {
    path: &'loader str,
    real_path: &'loader str,
  },
}

#[derive(Debug)]
pub struct SourceLocation {
  line: i32,
  column: i32,
}

#[derive(Debug)]
pub struct SourceRange {
  first_location: SourceLocation,
  last_location: SourceLocation,
}

#[derive(Debug)]
pub struct IseqLocation<'loader> {
  path: SourcePath<'loader>,
  base_label: &'loader str,
  label: &'loader str,
  first_line_number: u16,
  source_range: SourceRange,
}

#[derive(Debug, FromPrimitive)]
pub enum IseqType {
  Top = 0,
  Method = 1,
  Block = 2,
  Class = 3,
  Rescue = 4,
  Ensure = 5,
  Eval = 6,
  Main = 7,
  DefinedGuard = 8,
}

bitfield! {
  pub struct IseqParamFlags(u32);
  impl Debug;
  pub has_lead, _: 0;
  pub has_opt, _: 1;
  pub has_rest, _: 2;
  pub has_post, _: 3;
  pub has_kw, _: 4;
  pub has_kwrest, _: 5;
  pub has_block, _: 6;
  pub ambiguous_param0, _: 7;
}

#[derive(Debug)]
pub struct KwParam<'loader, 'source: 'loader> {
  pub name: &'source str,
  pub default_value: Option<Value<'loader, 'source>>,
}

#[derive(Debug)]
pub struct IseqParamSpec<'loader, 'source: 'loader> {
  pub size_lead: usize,
  pub size_opt: usize,
  pub has_rest: bool,
  pub size_post: usize,
  pub keywords: Vec<KwParam<'loader, 'source>>,
  pub has_kw_rest: bool,
  pub has_block: bool,
}

#[derive(Debug)]
pub struct Iseq<'loader, 'source: 'loader> {
  pub ty: IseqType,
  pub size: usize,
  pub _iseq_encoded: usize,
  pub param_spec: IseqParamSpec<'loader, 'source>,
  pub location: IseqLocation<'loader>,
  pub _insns_info: usize,
  pub _local_table: usize,
  pub _catch_table: usize,
  pub _parent_iseq: usize,
  pub _local_iseq: usize,
  pub _is_entries: usize,
  pub _ci_entries: usize,
  pub _cc_entries: usize,
  pub _mark_array: usize,
  pub _local_table_size: usize,
  pub _is_size: usize,
  pub _ci_size: usize,
  pub _ci_kw_size: usize,
  pub _insns_info_size: usize,
  pub _stack_max: usize,
}
