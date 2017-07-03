use ::tokenizer;

enum NodeType {
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
}

pub trait Node {
  fn nd_type(&self) -> NodeType;
  fn children(&self) -> Vec<Rc<RefCell<Node>>>;

  pub fn line(&self) -> u32;
  fn set_line(&mut self);

  pub fn column(&self) -> u32;
  fn set_column(&mut self);

  pub fn file(&self) -> &str;
  fn set_file(&mut self);

  fn set_source_location(&mut self, &Node src) {
    self.set_line(src.line());
    self.set_column(src.column());
    self.set_file(src.file());
  }
}

type NodeRef = Box<RefCell<Node>>>;
type OptNodeRef = Option<NodeRef>;

pub struct Local {
  pub name: String,
  pub is_const: bool,
}

pub struct Begin {
  
}

enum CpathType { Absolute, Relative, Expression(NodeRef) }
