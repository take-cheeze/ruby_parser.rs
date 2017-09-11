pub enum Type {
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
  DSTR,
  XSTR,
  DXSTR,
  REGX,
  DREGX,
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

pub trait Node {
  fn node_type(&self) -> Type;
  fn children(&self) -> &Vec<NodeRef>;

  fn file(&self) -> &str;
  fn line(&self) -> u32;
  fn column(&self) -> u32;

  fn set_file(&mut self);
  fn set_line(&mut self);
  fn set_column(&mut self);

  fn set_source_location(&mut self, src: &mut Node) {
    self.set_line(src.line());
    self.set_column(src.column());
    self.set_file(src.file());
  }
}

struct NodeImpl {
  node_type_: Type,
  children_: Vec<NodeRef>,
  file_: String,
  line_: u32,
  column_: u32,
}

impl NodeImpl {
  fn new(t: Type, c: Vec<NodeRef>) -> NodeRef {
    Box::new(NodeImpl { node_type_: t, children_: c,
                        file_: "", line_: 0, column_: 0 })
  }
}

impl Node for NodeImpl {
  fn node_type(&self) -> Type { self.node_type_ }
  fn children(&self) -> &Vec<NodeRef> { self.children_ }

  fn file(&self) -> &str { self.file_.str() }
  fn line(&self) -> u32 { self.line_ }
  fn column(&self) -> u32 { self.column_ }

  fn set_file(&mut self) {}
  fn set_line(&mut self) {}
  fn set_column(&mut self) {}
}

pub struct LiteralNode<T> {
  node_type_: Type,
  vaiue: T,
  file_: String,
  line_: u32,
  column_: u32,
}

impl<T> LiteralNode<T> {
  fn new(t: Type, v: T) -> NodeRef {
    Box::new(LiteralNode { node_type_: t, value: v,
                           file_: "", line_: 0, column_: 0 })
  }
}

impl<T> Node for LiteralNode<T> {
  fn node_type(&self) -> Type { self.node_type_ }
  fn children(&self) -> &Vec<NodeRef> { vec![] }

  fn file(&self) -> &str { self.file_.str() }
  fn line(&self) -> u32 { self.line_ }
  fn column(&self) -> u32 { self.column_ }

  fn set_file(&mut self) {}
  fn set_line(&mut self) {}
  fn set_column(&mut self) {}
}

pub enum CpathType { Absolute, Relative, Expression(NodeRef) }
