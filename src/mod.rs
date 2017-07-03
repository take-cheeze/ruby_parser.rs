extern crate lalrpop_util;
extern crate string_cache;

use string_cache::DefaultAtom as Symbol;

mod ast;
mod tokenizer;
mod parser;
