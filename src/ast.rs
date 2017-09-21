pub enum Kind {
  METHOD,
  FBODY,
  CFUNC,
  SCOPE,
  BLOCK,
  IF,
  CASE,
  WHEN,
  OPT_N,
  WHILE,
  UNTIL,
  ITER,
  FOR,
  BREAK,
  NEXT,
  REDO,
  RETRY,
  BEGIN,
  RESCUE,
  ENSURE,
  AND,
  OR,
  NOT,
  MASGN,
  ASGN,
  CDECL,
  CVASGN,
  CVDECL,
  OP_ASGN,
  CALL,
  SCALL,
  FCALL,
  VCALL,
  SUPER,
  ZSUPER,
  ARRAY,
  ZARRAY,
  HASH,
  RETURN,
  YIELD,
  LVAR,
  DVAR,
  GVAR,
  IVAR,
  CONST,
  CVAR,
  NTH_REF,
  BACK_REF,
  MATCH,
  MATCH2,
  MATCH3,
  INT,
  FLOAT,
  NEGATE,
  LAMBDA,
  SYM,
  STR,
  XSTR,
  REGX,
  DREGX_ONCE,
  LIST,
  ARG,
  ARGSCAT,
  ARGSPUSH,
  SPLAT,
  TO_ARY,
  SVALUE,
  BLOCK_ARG,
  DEF,
  SDEF,
  ALIAS,
  UNDEF,
  CLASS,
  MODULE,
  SCLASS,
  COLON2,
  COLON3,
  CREF,
  DOT2,
  DOT3,
  FLIP2,
  FLIP3,
  ATTRSET,
  SELF,
  NIL,
  TRUE,
  FALSE,
  DEFINED,
  NEWLINE,
  POSTEXE,
  ALLOCA,
  DMETHOD,
  BMETHOD,
  MEMO,
  IFUNC,
  DSYM,
  ATTRASGN,
  HEREDOC,
  LITERAL_DELIM,
  WORDS,
  SYMBOLS,
  BODY_STMT
}

pub type NodeRef = Box<Node>;
pub type OptNodeRef = Option<NodeRef>;

pub enum Value {
  Children(Vec<NodeRef>),
  Sym(::Symbol),
  Int(i32),
  Flt(Vec<u8>),
  Str(Vec<u8>),
}

pub struct Node {
  pub kind: Kind,
  pub value: Value,
  file_: String,
  line_: u32,
  column_: u32,
}

impl Node {
  pub fn new(t: Kind, c: Vec<NodeRef>) -> NodeRef {
    Box::new(Node { kind: t, value: Value::Children(c),
                        file_: String::new(), line_: 0, column_: 0 })
  }

  pub fn new_sym(v: ::Symbol) -> NodeRef {
    Box::new(Node { kind: Kind::SYM, value: Value::Sym(v),
                        file_: String::new(), line_: 0, column_: 0 })
  }

  pub fn new_int(t: Kind, v: i32) -> NodeRef {
    Box::new(Node { kind: t, value: Value::Int(v),
                        file_: String::new(), line_: 0, column_: 0 })
  }

  pub fn new_flt(t: Kind, v: Vec<u8>) -> NodeRef {
    Box::new(Node { kind: t, value: Value::Flt(v),
                        file_: String::new(), line_: 0, column_: 0 })
  }

  pub fn new_str(t: Kind, v: Vec<u8>) -> NodeRef {
    Box::new(Node { kind: t, value: Value::Str(v),
                        file_: String::new(), line_: 0, column_: 0 })
  }

  pub fn file(&self) -> &str { self.file_.as_str() }
  pub fn line(&self) -> u32 { self.line_ }
  pub fn column(&self) -> u32 { self.column_ }

  pub fn set_file(&mut self, f: &str) { self.file_.clear(); self.file_.insert_str(0, f); }
  pub fn set_line(&mut self, l: u32) { self.line_ = l; }
  pub fn set_column(&mut self, c: u32) { self.column_ = c; }

  pub fn set_source_location(&mut self, src: &mut Node) {
    self.set_line(src.line());
    self.set_column(src.column());
    self.set_file(src.file());
  }
}

pub enum CpathType { Absolute, Relative, Expression(NodeRef) }
