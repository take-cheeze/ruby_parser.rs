use ast::NodeRef;
use ast::Node;
use ast::Kind;
use ast::OptNodeRef;
use ast::CpathType;

use ::Symbol;

use combine::*;
use combine::Parser;
use combine::range::range;
use combine::byte::*;

macro_rules! nd {
  ($name:ident) => (
    Node::new(Kind::$name, vec![])
  );

  ($name:ident, $first:expr $(, $rest:expr)*) => (
    Node::new(Kind::$name, vec![$first $(,$rest)*])
  );

  ($name:ident; $children:expr) => (
    Node::new(Kind::$name, $children)
  );
}

fn 

macro_rules! kw {
  ($k:ident) => { (range(stringify!($k).as_bytes()[..]),
                   look_ahead(not_followed_by(one_of("!?=_".bytes())
                                              .or(alpha_num()))))
                   .map(|_| ())};
}

pub fn program<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = u8> { top_stmts().parse_stream(i) }

const SPACES: &'static str = " \t\x0c\r\x5c";

parser!{
  fn ws[I]()(I) -> () where [I: Stream<Item = u8>] {
    many1(one_of(SPACES.bytes())).map(|_| ())
  }
}

parser!{
  fn opt_ws[I]()(I) -> () where [I: Stream<Item = u8>] {
    many(one_of(SPACES.bytes())).map(|_| ())
  }
}

parser!{
  fn ws_nl[I]()(I) -> () where [I: Stream<Item = u8>] {
    many1(one_of(SPACES.bytes()).or(nl))
  }
}

parser!{
  fn opt_ws_nl[I]()(I) -> () where [I: Stream<Item = u8>] {
    many(one_of(SPACES.bytes()).or(nl))
  }
}

parser!{
  fn comma[I]()(I) -> () where [I: Stream<Item = u8>] {
    (opt_ws(), token(b','), opt_ws_nl())
  }
}

parser!{
  fn ident[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    (token(b'_').or(lower()), many(token(b'_').or(alpha_num())))
      .map(|v| Symbol::from(v))
  }
}

parser!{
  fn cvar[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    (range(&b"@@"[..]), token(b'_').or(lower()), many(token(b'_').or(alpha_num())))
      .map(|v| Symbol::from(v))
  }
}

parser!{
  fn ivar[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    (token(b'@'), token(b'_').or(lower()), many(token(b'_').or(alpha_num())))
      .map(|v| Symbol::from(v))
  }
}

parser!{
  fn gvar[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    (token(b'$'), token(b'_').or(lower()), many(token(b'_').or(alpha_num())))
      .map(|v| Symbol::from(v))
  }
}

parser!{
  fn fid[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    (token(b'_').or(lower()), many(token(b'_').or(alpha_num())), optional(one_of("!?=".chars())))
      .map(|v| Symbol::from(v))
  }
}

parser!{
  fn constant[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    (token(b'_').or(letter()), many(token(b'_').or(alpha_num())))
      .map(|v| Symbol::from(v))
  }
}

fn call_uni_op(recv: NodeRef, op: Symbol) -> NodeRef {
  nd!(CALL, recv, Node::new_sym(Kind::SYM, op))
}

fn call_bin_op(l: NodeRef, op: Symbol, r: NodeRef) -> NodeRef {
  nd!(CALL, l, Node::new_sym(Kind::SYM, op), r)
}

fn call_with_block(n: NodeRef, b: NodeRef) -> NodeRef {
  nd!(BLOCK_ARG, n, b)
}

parser!{
  fn top_stmts[I]()(I) -> Vec<NodeRef> where [I: Stream<Item = u8>] {
    sep_end_by(stmt, term)
  }
}

parser!{
  fn top_stmt[I]()(I) -> Vec<NodeRef> where [I: Stream<Item = u8>] {
    choice!(
      stmt,
      (kw!(BEGIN), opt_ws, token(b'{'),
       opt_ws, top_stmts,
       opt_ws, tokens('}'))
        .map(|(_, _, _, _, stmts, _, _)| stmts )
    )
  }
}

parser!{
  fn bodystmt[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (compstmt, optional(rescue), optional(else_),
     optional(kw!(ensure), compstmt))
      .map(|(c, r, el, en)| nd!(BODY_STMT, c, r, el, en))
  }
}

parser!{
  fn compstmt[I]()(I) -> NodeRef where [I: Stream<Item = u8>] { stmts() }
}

parser!{
  fn stmts[I]()(I) -> Vec<NodeRef> where [I: Stream<Item = u8>] {
    sep_end_by(stmt(), term())
  }
}

parser!{
  fn assign_op[I]()(I) -> () where [I: Stream<Item = u8>] {
    (opt_ws_nl(), token(b'='), opt_ws_nl()).map(|_| ())
  }
}

parser!{
  fn stmt_no_mod[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice!(
      (kw!(alias), opt_ws_nl(), fsym, opt_ws_nl(), fsym)
        .map(|(_, _, _, a, f)| nd!(ALIAS, a, f)),
      (kw!(undef), undef_list).map(|(_, l)| nd!(UNDEF, l)),
      command_asgn,
      (kw!(END), opt_ws, token(b'{'),
       opt_ws_nl(), compstmt, opt_ws_nl(),
       token(b'}')).map(|(_, _, _, _, stmt, _, _)| stmt),
      (mlhs, assign_op, command_call)
        .map(|(l, _, c)| nd!(MASGN, l, c)),
      (lhs, assign_op, mrhs)
        .map(|(l, _, r)| nd!(ARRAY, r)),
      (mlhs, assign_op, arg)
        .map(|(l, _, a)| nd!(MASGN, l, a)),
      (mlhs, assign_op, mrhs)
        .map(|(l, _, r)| nd!(MASGN, l, nd!(ARRAY, r))),
      expr
    )
  }
}

parser!{
  fn stmt[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice!(
      (stmt_no_mod, ws, kw!(if), opt_ws_nl(),
       stmt_no_mod).map(|(s, _, _, _, r)| nd!(UNLESS, r, s, None)),
      (stmt_no_mod, ws, kw!(unless), opt_ws_nl(),
       stmt_no_mod).map(|(s, _, _, _, r)| nd!(UNLESS, r, s, None)),
      (stmt_no_mod, ws, kw!(while), opt_ws_nl(),
       stmt_no_mod).map(|(s, _, _, _, r)| nd!(UNLESS, r, s, None)),
      (stmt_no_mod, ws, kw!(until), opt_ws_nl(),
       stmt_no_mod).map(|(s, _, _, _, r)| nd!(UNLESS, r, s, None)),
      stmt_no_mod)
  }
}

parser!{
  fn op_asgn[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    (opt_ws(),
     choice!(range(&b"*="[..]),
             range(&b"/="[..]),
             range(&b"+="[..]),
             range(&b"-="[..]),
             range(&b"%="[..]),
             range(&b"<<="[..]),
             range(&b">>="[..]),
             range(&b"&="[..]),
             range(&b"|="[..]),
             range(&b"^="[..]),
             range(&b"||="[..]),
             range(&b"&&="[..]))
     .map(|v| Symbol::from(v.get(..(v.len() - 1)))),
     opt_ws_nl()
    ).map(|(_, v, _)| v)
  }
}

parser!{
  fn command_asgn[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice!(
      (lhs, assign_op, command_rhs).map(|(l, _, r)| nd!(ASGN, l, r)),
      (var_lhs, op_asgn, command_rhs).map(|(l, op, r)| nd!(OP_ASGN, l, op, r)),
      (primary, opt_ws, token(b'['), opt_ws_nl(),
       opt_call_args, opt_ws_nl(), rbracket,
       opt_ws, op_asgn, opt_ws_nl(), command_rhs)
        .map(|(prim, _, _, _, idx, _, _, _, op, _, r)|
             nd!(OP_ASGN, nd!(CALL, prim, Symbol::from("[]"), idx, Symbol::from(".")), op, r)),
      (primary, call_op, ident, op_asgn, command_rhs)
        .map(|(prim, c, id, op, r)| nd!(OP_ASGN, nd!(CALL, prim, id, vec![], c), op, r)),
      (primary, call_op, constant, op_asgn, command_rhs)
        .map(|(prim, c, id, op, r)| nd!(OP_ASGN, nd!(CALL, prim, id, vec![], c), op, r)),
      (primary, opt_ws, "::", opt_ws, ident, op_asgn, command_rhs)
        .map(|(prim, _, _, _, id, op, r)|
             nd!(OP_ASGN, nd!(CALL, prim, id, vec![], Symbol::from("::"), op, r))))
  }
}

parser!{
  fn command_rhs[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice!(command_call(), command_asgn())
  }
}

parser!{
  fn expr_not[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice!(
      (kw!(not), ws_nl, arg).map(|(_, _, e)| call_uni_op(e, "!")),
      arg())
  }
}

parser!{
  fn expr_and_or[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    chainl1(expr_not,
            (opt_ws,
             choice!(
               kw!(and).map(|_| |l: NodeRef, r: NodeRef| nd!(AND, l, r)),
               kw!(or).map(|_| |l: NodeRef, r: NodeRef| nd!(OR, l, r)))),
            opt_ws_nl()).map(|(_, v, _)| v)
  }
}

parser!{
  fn expr[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice!(
      command_call,
      expr_and_or,
      (range(&b"!"), opt_ws_nl(), command_call).map(|(_, _, c)| call_uni_op(c, "!"[..])))
  }
}

parser!{
  fn command_call[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice![command(), block_command]
  }
}

parser!{
  fn block_command[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice![block_call,
            (block_call, call_op2, operation2, ws, command_args)
            .map(|(b, c, o, _, args)| nd!(CALL, b, c, o, args))]
  }
}

parser!{
  fn command[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (operation, ws, command_args).map(|(op, _, c)| nd!(FCALL, op, c))
      .or((primary, call_op, operation2, ws, command_args)
          .map(|(prim, c_op, op, _, c)| nd!(CALL, prim, op, c, c_op)))
      .or((primary, opt_ws, "::", opt_ws_nl(), operation2, command_args)
          .map(|(prim, _, _, _, op, c)| nd!(CALL, prim, op, c, Symbol::from("::"))))
      .or((kw!(super), ws_nl, command_args).map(|(_, _, c)| nd!(SUPER, c)))
      .or((kw!(yield), ws_nl, command_args).map(|(_, _, c)| nd!(YIELD, c)))
      .or((kw!(super), ws_nl, command_args).map(|(_, _, c)| nd!(SUPER, c)))
      .or((kw!(return), ws_nl, call_args).map(|(_, _, c)| nd!(RETURN, c)))
      .or((kw!(break), ws_nl, call_args).map(|(_, _, c)| nd!(BREAK, c)))
      .or((kw!(next), ws_nl, call_args).map(|(_, _, c)| nd!(NEXT, c)))
  }
}

parser!{
  fn mlhs[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice![mlhs_basic
            (token(b'('), mlhs_inner(), rparen()).map(|(_, mlhs, _)| mlhs)]
  }
}

parser!{
  fn mlhs_inner[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    parser(mlhs_basic)
      .or((token(b'('), mlhs_inner, rparen).map(|(_, mlhs, _)| mlhs))
  }
}

parser!{
  fn mlhs_basic[I]()(I) -> Vec<NodeRef> where [I: Stream<Item = u8>] {
    (many((mlhs_item, opt_ws, token(b','), opt_ws_nl()).map(|(v, _, _, _)| v)),
     opt_ws_nl(), token(b'*'), opt_ws, optional(lhs),
     many((opt_ws, token(b','), opt_ws_nl(), mlhs_item).map(|(_, _, _, v)| v))
     .map(|(pre, _, _, _, ary, post)| { pre.push(ary); pre.concat(post) }))
      .or(sep_by(mlhs_item, (opt_ws, token(b','), opt_ws_nl())))
  }
}

parser!{
  fn mlhs_item[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    parser(lhs)
      .or((token(b'('), opt_ws, mlhs_inner, rparen).map(|(_, _, v, _)| nd!(MASGN, v, vec![])))
  }
}

parser!{
  fn lhs[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    variable.map(|v| v)
      .or((primary, opt_ws, token(b'['), opt_ws, opt_call_args, ws, rbracket)
          .map(|(prim, _, _, c, _, _)| nd!(CALL, prim, Symbol::from("[]"), c, Symbol::from("."))))
      .or((primary, opt_ws_nl(), call_op, opt_ws_nl(), ident)
          .map(|(prim, _, c, _, id)| nd!(CALL, prim, id, vec![], c)))
      .or((primary, opt_ws, range(&b"::"[..]), opt_ws_nl(), ident)
          .map(|(prim, _, _, _, id)| nd!(CALL, prim, id, vec![], Symbol::from("::"))))
      .or((primary, opt_ws_nl(), call_op, opt_ws_nl(), constant)
          .map(|(prim, _, c, _, id)| nd!(CALL, prim, id, vec![], c)))
      .or((primary, opt_ws_nl(), range(&b"::"[..]), opt_ws_nl(), constant)
          .map(|(prim, _, c, _, id)| nd!(CALL, prim, id, vec![], Symbol::from("::"))))
      .or((range(&b"::"[..]), opt_ws_nl(), constant)
          .map(|(_, _, id)| nd!(COLON3, id)))
  }
}

parser!{
  fn cname[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    constant
  }
}

parser!{
  fn cpath[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (range(&b"::"[..]), opt_ws_nl(), cname).map(|(_, _, name)| nd!(ABSOLUTE_NAME, name))
      .or(cname)
      .or((primary, opt_ws, range(&b"::"[..]), opt_ws_nl(), cname)
          .map(|(prim, _, _, _, name)| nd!(CONST_REF, prim, name)))
  }
}

parser!{
  fn fname[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    choice!(ident(), constant(), fid(), op(), reswords())
  }
}

parser!{
  fn fsym[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    choice!(fname(), basic_symbol())
  }
}

parser!{
  fn undef_list[I]()(I) -> Vec<Symbol> where [I: Stream<Item = u8>] {
    sep_by(fsym, (opt_ws, token(b','), opt_ws_nl()))
  }
}

parser!{
  fn op[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    choice!(
      range(&b"|"[..]),
      range(&b"^"[..]),
      range(&b"&"[..]),
      range(&b"<=>"[..]),
      range(&b"=="[..]),
      range(&b"==="[..]),
      range(&b"=~"[..]),
      range(&b"!~"[..]),
      range(&b">"[..]),
      range(&b">="[..]),
      range(&b"<"[..]),
      range(&b"<="[..]),
      range(&b"!="[..]),
      range(&b"<<"[..]),
      range(&b">>"[..]),
      range(&b"+"[..]),
      range(&b"-"[..]),
      range(&b"*"[..]),
      range(&b"/"[..]),
      range(&b"%"[..]),
      range(&b"**"[..]),
      range(&b"!"[..]),
      range(&b"~"[..]),
      range(&b"+@"[..]),
      range(&b"-@"[..]),
      range(&b"[]"[..]),
      range(&b"[]="[..]),
      range(&b"`"[..]))
      .map(|v| Symbol::from(v.to_string()))
  }
}

parser!{
  fn reswords[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    (range(&b"__LINE__"[..])
     .or(range(&b"__FILE__"[..]))
     .or(range(&b"__ENCODING__"[..]))
     .or(kw!(BEGIN))
     .or(kw!(END))
     .or(kw!(alias))
     .or(kw!(and))
     .or(kw!(begin))
     .or(kw!(break))
     .or(kw!(case))
     .or(kw!(class))
     .or(kw!(def))
     .or(kw!(do))
     .or(kw!(else))
     .or(kw!(elsif))
     .or(kw!(end))
     .or(kw!(ensure))
     .or(kw!(false))
     .or(kw!(for))
     .or(kw!(in))
     .or(kw!(module))
     .or(kw!(next))
     .or(kw!(nil))
     .or(kw!(not))
     .or(kw!(or))
     .or(kw!(redo))
     .or(kw!(rescue))
     .or(kw!(retry))
     .or(kw!(return))
     .or(kw!(self))
     .or(kw!(super))
     .or(kw!(then))
     .or(kw!(true))
     .or(kw!(undef))
     .or(kw!(when))
     .or(kw!(yield))
     .or(kw!(if))
     .or(kw!(unless))
     .or(kw!(while))
     .or(kw!(until))
    )
      .not_followed_by(one_of("!?=_".char()).or(alpha_num()))
      .map(|v| Symbol::from(v))
  }
}

parser!{
  fn arg1[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (many((token(b'!').or(token(b'~')).or(token(b'+')), opt_ws_nl()).map(|(op, _)| op)),
     primary)
      .map(|(op, _)| op)
  }
}

parser!{
  fn arg2[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    chainr1(arg1,
            range(&b"**"[..]).map(|op| |l: NodeRef, r: NodeRef| call_bin_op(l, "**", r)))
  }
}

parser!{
  fn arg3[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (many((token(b'-'), opt_ws_nl()).map(|(op, _)| op)), arg2)
      .map(|(op, a)| call_uni_op(a, "-"))
  }
}

parser!{
  fn arg4[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    chainl1(arg3(), (opt_ws(), one_of(b"*/%".bytes()), opt_ws_nl()))
  }
}

parser!{
  fn arg5[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    chainl1(arg4, (opt_ws, one_of(b"+-".bytes()), opt_ws_nl()))
  }
}

parser!{
  fn arg6[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    chainl1(arg5, (opt_ws, range(&b"<<").or(range(">>"[..])), opt_ws_nl()))
  }
}

parser!{
  fn arg7[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    chainl1(arg6, (opt_ws, token(b'&'), opt_ws_nl()))
  }
}

parser!{
  fn arg8[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    chainl1(arg7, (opt_ws, token(b'|').or(token(b'~')), opt_ws_nl()))
  }
}

parser!{
  fn arg9[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    chainl1(arg8, (opt_ws,
                   range(&b">"[..])
                   .or(range(&b">="[..]))
                   .or(range(&b"<"[..]))
                   .or(range(&b"<="[..])), opt_ws_nl()))
  }
}

parser!{
  fn arg10[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    chainl1(arg9, (opt_ws,
                   range(&b"<=>"[..])
                   .or(range(&b"=="[..]))
                   .or(range(&b"==="[..]))
                   .or(range(&b"!="[..]))
                   .or(range(&b"=~"[..]))
                   .or(range(&b"!~"[..])),
                   opt_ws_nl()))
  }
}

parser!{
  fn arg11[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    chainl1(arg10, (opt_ws, range(&b"&&"[..]), opt_ws_nl()))
  }
}

parser!{
  fn arg12[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    chainl1(arg11, (opt_ws, range(&b"||"[..]), opt_ws_nl()))
  }
}

parser!{
  fn arg13[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    chainl1(arg12, (opt_ws, range(&b"..").or(range("..."[..])), opt_ws_nl()))
  }
}

parser!{
  fn arg14[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (arg13, opt_ws, token(b'?'), opt_ws_nl(), arg, opt_ws_nl(), token(b':'), opt_ws_nl(), arg)
      .map(|(c, _, _, _, t, _, _, _, f)| nd!(IF, c, t, f))
      .or(arg13)
  }
}

parser!{
  fn arg_rhs[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    parser(arg14)
      .or((arg_rhs, ws, kw!(rescue), ws_nl, arg14)
          .map(|(a, _, _, _, r)| nd!(MOD_RESCUE, a, r)))
  }
}

parser!{
  fn arg[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (lhs, opt_ws, token(b'='), opt_ws_nl(), arg_rhs).map(|(l, _, _, _, r)| nd!(ASGN, l, r))
      .or((var_lhs, opt_ws, op_asgn, opt_ws_nl(), arg_rhs).map(|(l, _, op, _, r)| nd!(OP_ASGN, l, op, r)))
      .or((primary, opt_ws, token(b'['), opt_ws_nl(), opt_call_args, opt_ws_nl(), rbracket,
           opt_ws, op_asgn, opt_ws_nl(), arg_rhs)
          .map(|(prim, _, _, _, args, _, _, _, op, r)|
               nd!(OP_ASGN, nd!(CALL, prim, Symbol::from("[]"), args), op, r)))
      .or((primary, opt_ws_nl(), call_op, opt_ws_nl(), ident, opt_ws, op_asgn, opt_ws_nl(), arg_rhs)
          .map(|(prim, _, c_op, _, id, _, op, _, r)| nd!(OP_ASGN, nd!(CALL, prim, id, vec![], c_op), op, r)))
      .or((primary, opt_ws_nl(), call_op, opt_ws_nl(), constant, opt_ws, op_asgn, opt_ws_nl(), arg_rhs)
          .map(|(prim, _, c_op, _, id, _, op, _, r)| nd!(OP_ASGN, nd!(CALL, prim, id, vec![], c_op), op, r)))
      .or((primary, opt_ws_nl(), range(&b"::"[..]), opt_ws_nl(), ident, opt_ws, op_asgn, opt_ws_nl(), arg_rhs)
          .map(|(prim, _, _, _, id, _, op, _, r)|
               nd!(OP_ASGN, nd!(CALL, prim, id, vec![], Symbol::from("::")), op, r)))
      .or(arg14)
  }
}

parser!{
  fn trailer[I]()(I) -> () where [I: Stream<Item = u8>] {
    (opt_ws_nl())
      .or((opt_ws, token(b','), opt_ws))
  }
}

parser!{
  fn aref_args[I]()(I) -> Vec<NodeRef> where [I: Stream<Item = u8>] {
    (sep_by1(arg, comma_hd),
     optional((comma_hd, "*", opt_ws_nl(), arg).map(|(_, _, _, a)| a)),
     optional((comma_hd, assoc).map(|(_, h)| h)),
     optional(comma_hd))
      .map(|(args, sp, h, _)| {
        match sp { Some(sp) => { args.push(nd!(SPLAT, sp)); }, None => {} };
        match h { Some(h) => { args.push(nd!(HASH, h)); }, None => {} };
        args
      })
      .or((
        ("*", opt_ws_nl(), arg).map(|(_, _, a)| a),
        optional((comma_hd, assoc).map(|(_, h)| h)),
        optional(comma_hd))
          .map(|(sp, h, _)| {
            let args = vec![sp];
            match h { Some(h) => { args.push(nd!(HASH, h)); }, None => {} };
            args
          }))
      .or((
        assoc.map(|(_, h)| h),
        optional(comma_hd))
          .map(|(h, _)| vec![nd!(HASH, h)]))
      .or(opt_ws_nl().map(|_| vec![]))
  }
}

parser!{
  fn paren_args[I]()(I) -> Vec<NodeRef> where [I: Stream<Item = u8>] {
    (token(b'('), opt_ws_nl(), opt_call_args, rparen)
      .map(|(_, _, a, _)| a)
  }
}

parser!{
  fn opt_call_args[I]()(I) -> Vec<NodeRef> where [I: Stream<Item = u8>] {
    (call_args, optional(comma_hd))
      .map(|(args, _)| args)
  }
}

parser!{
  fn call_args[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    parser(command).map(|v| vec![v])
      .or((sep_by1(arg, comma_hd),
           optional((comma_hd, token(b'*'), arg).map(|(_, _, sp)| nd!(SPLAT, sp))),
           many((comma_hd, arg).map(|(_, a)| a)),
           optional((comma_hd, assoc).map(|(_, h)| nd!(HASH, h))),
           optional((comma_hd, blockarg).map(|(_, b)| b)))
          .map(|(args, sp, post, h, b)| {
            match sp { Some(sp) => { args.push(sp); }, None => {} };
            args.append(post);
            match h { Some(h) => { args.push(h); }, None => {} };
            match b { Some(b) => { args.push(b); }, None => {} };
            args
          }))
      .or(((token(b'*'), arg).map(|(_, sp)| nd!(SPLAT, sp)),
           many((comma_hd, arg).map(|(_, a)| a)),
           optional((comma_hd, assoc).map(|(_, h)| nd!(HASH, h))),
           optional((comma_hd, blockarg).map(|(_, b)| b)))
          .map(|(sp, args, h, b)| {
            match sp { Some(sp) => { args.insert(0, sp); }, None => {} };
            match h { Some(h) => { args.push(h); }, None => {} };
            match b { Some(b) => { args.push(b); }, None => {} };
            args
          }))
      .or((assoc.map(|(_, h)| h),
           optional((comma_hd, blockarg).map(|(_, b)| b)))
          .map(|(h, b)| {
            let mut args = vec![];
            match h { Some(h) => { args.push(nd!(HASH, h)); }, None => {} };
            match b { Some(b) => { args.push(b); }, None => {} };
            args
          }))
      .or((comma_hd, blockarg).map(|(_, b)| vec![b]))
  }
}

parser!{
  fn command_args[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    call_args
  }
}

parser!{
  fn blockarg[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (token(b'&'), opt_ws_nl(), arg).map(|(_, _, a)| nd!(BLOCK_ARG, a))
  }
}

parser!{
  fn comma_hd[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (opt_ws, token(b','), opt_ws_nl())
  }
}

parser!{
  fn mrhs[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    sep_by1(arg(), comma())
      .or((sep_end_by(arg(), comma()),
           token(b'*'), opt_ws, arg).map(|(args, _, _, sp)| {
             args.push(nd!(SPLAT, sp));
             args
           }))
  }
}

parser!{
  fn primary[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice![
      literal(),
      var_ref(),
      backref(),
      fid().map(|v| nd!(FCALL, Node::new_sym(Kind::SYM, v))),
      between((kw!(begin), ws_nl()), (opt_ws_nl(), kw!(end)), bodystmt()),
      between((token(b'('), opt_ws_nl()), (opt_ws_nl(), token(b')')), stmt()),
      (token(b'('), opt_ws_nl(), token(b')')).map(|(_, _, _)| nd!(NIL)),
      between((token(b'('), opt_ws_nl()), (opt_ws_nl(), token(b')')), compstmt()),
      (primary, opt_ws, range(&b"::"[..]), opt_ws_nl(), constant())
        .map(|(prim, _, _, _, id)| nd!(COLON2, prim, id)),
      (range(&b"::"[..]), opt_ws_nl(), constant()).map(|(_, _, id)| nd!(COLON3, id)),
      between((token(b'['), opt_ws_nl()), (opt_ws_nl(), token(b']')),
              aref_args().map(|v| nd!(ARRAY; v))),
      kw!(return).map(|_| nd!(RETURN)),
      (kw!(yield), ws(), paren_args()).map(|(_, _, args)| nd!(YIELD; args)),
      (kw!(not), ws(), between((token(b'('), opt_ws_nl()), (opt_ws_nl(), token(b')')),
                             optional(expr())))
        .map(|(_, _, e)| call_uni_op(match e { Some(e) => e, None => nd!(NIL) },
                                     Symbol::from("!"))),
      (operation(), opt_ws_nl(), brace_block()).map(|(op, _, b)| nd!(FCALL, op, b)),
      (method_call(), opt_ws_nl(), brace_block()).map(|(op, _, b)| call_with_block(op, b)),
      method_call(),
      (range(&b"->"[..]), opt_ws_nl(), f_larglist(), opt_ws_nl(), lambda_body())
        .map(|(_, _, a, _, b)| nd!(LAMBDA, a, b)),
      (kw!(if), opt_ws_nl(), expr, opt_ws, then, opt_ws_nl(), compstmt(), opt_ws_nl(),
       if_tail, opt_ws_nl(), kw!(end))
        .map(|(_, _, c, _, _, _, s, _, t)| nd!(IF, c, s, t)),
      (kw!(unless), opt_ws_nl(), expr(), opt_ws(), then(), opt_ws_nl(), compstmt(), opt_ws_nl(),
       else_(), opt_ws_nl(), kw!(end))
        .map(|(_, _, c, _, _, _, s, _, t)| nd!(UNLESS, c, s, t)),
      (kw!(while), opt_ws_nl(), expr(), opt_ws(), do_(), opt_ws_nl(), compstmt(), opt_ws_nl(), kw!(end))
        .map(|(_, _, c, _, _, _, s, _, t)| nd!(WHILE, c, s, t)),
      (kw!(until), opt_ws_nl(), expr(), opt_ws(), do_(), opt_ws_nl(), compstmt(), opt_ws_nl(), kw!(end))
        .map(|(_, _, c, _, _, _, s, _, t)| nd!(UNTIL, c, s, t)),
      (kw!(case), opt_ws_nl(), expr(), many(term()), opt_ws_nl(), case_body(), opt_ws_nl(), kw!(end))
        .map(|(_, _, c, _, _, _, s, _, t)| nd!(CASE, c, s, t)),
      (kw!(case), opt_ws_nl(), opt_ws_nl(), case_body(), opt_ws_nl(), kw!(end))
        .map(|(_, _, c, _, _, _, s, _, t)| nd!(CASE, c, s, t)),
      (kw!(for), opt_ws_nl(), for_var, opt_ws_nl(), kw!(in), opt_ws_nl(), expr,
       opt_ws_nl(), do_(), opt_ws_nl(), compstmt(), kw!(end))
        .map(|(_, _, c, _, _, _, s, _, t)| nd!(FOR, c, t, s)),
      (kw!(class), opt_ws_nl(), cpath, opt_ws_nl(), superclass, many(term),
       bodystmt, opt_ws_nl(), kw!(end)).map(|(_, _, c, _, s, _, b, _, _)| nd!(CLASS, c, s, b)),
      (kw!(class), opt_ws_nl(), range(&b"<<"[..]), opt_ws_nl(), expr, many(term),
       bodystmt, opt_ws_nl(), kw!(end)).map(|(_, _, c, _, s, _, b, _, _)| nd!(SCLASS, c, b)),
      (kw!(module), opt_ws_nl(), cpath, many(term),
       bodystmt, opt_ws_nl(), kw!(end)).map(|(_, _, c, _, s, _, b, _, _)| nd!(SCLASS, c, b)),
      (kw!(def), opt_ws_nl(), fname, ws, farglist, term, bodystmt, opt_ws_nl(), kw!(end))
        .map(|(_, _, f, _, args, _, b, _, _)| nd!(DEF, f, args, b)),
      kw!(break).map(|_| nd!(BREAK)),
      kw!(next).map(|_| nd!(NEXT)),
      kw!(redo).map(|_| nd!(REDO)),
      kw!(retry).map(|_| nd!(RETRY))]
  }
}

parser!{
  fn then[I]()(I) -> () where [I: Stream<Item = u8>] {
    (term, optional(kw!(then)))
      .or(kw!(then))
  }
}

parser!{
  fn do_[I]()(I) -> () where [I: Stream<Item = u8>] {
    parser(term)
      .or((ws, kw!(do), ws_nl))
  }
}

parser!{
  fn else_[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (kw!(else), ws_nl, compstmt).map(|(_, _, st)| nd!(ELSE, st))
  }
}

parser!{
  fn if_tail[I]()(I) -> Vec<NodeRef> where [I: Stream<Item = u8>] {
    (many((kw!(elsif), ws, expr, opt_ws, then, opt_ws, compstmt)
          .map(|(_, _, cond, _, _, _, st)| nd!(ELSIF, cond, compstmt))),
     opt_ws_nl(),
     optional(else_))
      .map(|(elifs, _, el)| {
        match el { Some(el) => { elifs.push(el); }, None => {} };
        elifs
      })
  }
}

parser!{
  fn for_var[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    parser(lhs).or(mlhs)
  }
}

parser!{
  fn f_marg[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    parser(ident).map(|v| nd!(ARG, v))
      .or(between((token(b'('), opt_ws_nl()), (opt_ws_nl(), token(b')')),
                  f_margs.map(|v| nd!(MASGN, v, vec![]))))
  }
}

parser!{
  fn f_margs[I]()(I) -> (Vec<NodeRef>, Option<OptNodeRef>, Vec<NodeRef>) where [I: Stream<Item = u8>]
  {
    sep_by1(f_marg, (opt_ws_nl(), token(b','), opt_ws_nl()))
      .map(|v| (v, None, vec![]))
      .or((many((f_marg, opt_ws, token(b','), opt_ws_nl()).map(|(v, _, _, _)| v)),
           token(b'*'), opt_ws_nl(), optional(ident), opt_ws,
           many((token(b','), opt_ws_nl(), f_marg, opt_ws).map(|(v, _, _, _)| v)))
          .map(|(pre, _, _, rest, _, post)| (pre, Some(rest), post)))
  }
}

parser!{
  fn block_param_def[I]()(I) -> (Vec<NodeRef>, Vec<NodeRef>) where [I: Stream<Item = u8>] {
    range(&b"||"[..]).map(|v| (vec![], vec![]))
      .or((token(b'|'), opt_ws_nl(), block_param, optional(bvars), opt_ws_nl(), token(b'|')))
  }
}

parser!{
  fn bvars[I]()(I) -> Vec<NodeRef> where [I: Stream<Item = u8>] {
    (opt_ws_nl(), token(b';'), sep_by1(bvar, (opt_ws_nl(), token(b','), opt_ws_nl())))
      .map(|(_, _, v)| v)
  }
}

parser!{
  fn bvar[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    parser(ident).map(|v| nd!(BV, v))
  }
}

parser!{
  fn f_larglist[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (token(b'('), opt_ws_nl(), fargs, opt_ws_nl(), optional(bvars), opt_ws_nl(), token(b')'))
      .map(|(_, _, v, _, b, _)| v)
      .or(fargs)
  }
}

parser!{
  fn lambda_body[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (token(b'{'), opt_ws_nl(), compstmt, opt_ws_nl(), token(b'}')).map(|(_, _, v, _, _)| v)
      .or((kw!(do), opt_ws_nl(), compstmt, opt_ws_nl(), kw!(end)).map(|(_, _, v, _, _)| v))
  }
}

parser!{
  fn do_block[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (kw!(do), opt_ws_nl(), optional(block_param_def), opt_ws_nl(), compstmt, opt_ws_nl(), kw!(end))
      .map(|(_, _, b, _, v, _, _)| v)
  }
}

parser!{
  fn block_call[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (command, opt_ws, do_block).map(|(c, _, b)| call_with_block(c, b))
      .or((block_call, opt_ws_nl(), call_op2, opt_ws_nl(), operation2, optional(paren_args))
          .map(|(b, _, c_op, _, op, a)| nd!(CALL, b, op, a, c_op)))
      .or((block_call, opt_ws_nl(), call_op2, opt_ws_nl(), operation2, optional(paren_args),
           opt_ws_nl(), brace_block)
          .map(|(b, _, c_op, _, op, a, _, br)| call_with_block(nd!(CALL, b, op, a, c_op), br)))
      .or((block_call, opt_ws_nl(), call_op2, opt_ws_nl(), operation2, optional(paren_args),
           opt_ws_nl(), do_block)
          .map(|(b, _, c_op, _, op, a, _, br)| call_with_block(nd!(CALL, b, op, a, c_op))))
  }
}

parser!{
  fn method_call[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (operation, paren_args).map(|(op, a)| nd!(FCALL, op, a))
      .or((primary, opt_ws_nl(), call_op, opt_ws_nl(), operation2, optional(paren_args))
          .map(|(prim, _, c_op, _, op, a)| nd!(CALL, prim, op, a, c_op)))
      .or((primary, opt_ws_nl(), range(&b"::"[..]), opt_ws_nl(), operation2, paren_args)
          .map(|(prim, _, c_op, _, op, a)| nd!(CALL, prim, op, a, c_op)))
      .or((primary, opt_ws_nl(), range(&b"::"[..]), opt_ws_nl(), operation3)
          .map(|(prim, _, c_op, _, op)| nd!(CALL, prim, op, vec![], "::")))
      .or((primary, opt_ws_nl(), call_op, paren_args)
          .map(|(prim, _, c_op, a)| nd!(CALL, prim, Symbol::from("call"), a, c_op)))
      .or((primary, opt_ws_nl(), "::", paren_args)
          .map(|(prim, _, c_op, a)| nd!(CALL, prim, Symbol::from("call"), a, "::")))
      .or((kw!(super), opt_ws, paren_args)
          .map(|(_, _, a)| nd!(SUPER, a)))
      .or((kw!(super))
          .map(|(_, _, a)| nd!(ZSUPER)))
      .or((primary, opt_ws, token(b'['), opt_ws_nl(), opt_call_args, rbracket)
          .map(|(prim, _, _, _, a, _)| nd!(CALL, prim, Symbol::from("[]"), a, ".")))
  }
}

parser!{
  fn brace_block[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (token(b'{'), opt_ws, optional(block_param_def), opt_ws_nl(),
     compstmt, opt_ws_nl(), token(b'}').map(|(_, _, param, _, s, _, _)| nd!(BLOCK, param, s)))
      .or((kw!(do), opt_ws, optional(block_param_def), opt_ws_nl(),
           compstmt, opt_ws_nl(), kw!(end))
          .map(|(_, _, param, _, s, _, _)| nd!(BLOCK, param, s)))
  }
}

parser!{
  fn case_body[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (kw!(when), opt_ws_nl(), sep_by(arg, (opt_ws, token(b','), opt_ws_nl())),
     opt_ws, then, opt_ws_nl(), compstmt, cases)
      .map(|(_, _, l, _, _, s, c)| (l, s, Some(c)))
  }
}

parser!{
  fn cases[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (else_).map(|v| (None, v, None))
      .or(case_body)
  }
}

parser!{
  fn rescue[I]()(I) -> Vec<NodeRef> where [I: Stream<Item = u8>] {
    sep_by1((kw!(rescue), ws, exc_list, opt_ws, optional((range(&b"=>"[..]), opt_ws_nl(), lhs)),
             opt_ws, then, opt_ws_nl(), compstmt)
            .map(|(_, _, l, _, v, _, _, s)| (l, v, s)), nl)
  }
}

parser!{
  fn exc_list[I]()(I) -> Vec<NodeRef> where [I: Stream<Item = u8>] {
    (arg).map(|v| vec![v])
      .or(mrhs)
  }
}

parser!{
  fn literal[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice![numeric(), symbol(), string_literal()]
  }
}

fn get_lit_end(c: u8) -> u8 {
  assert!(!::ascii::AsciiChar::from(c).is_alphanumeric());

  match c {
    b'(' => b')',
    b'[' => b']',
    b'{' => b'}',
    b'<' => b'>',
    c => c,
  }
}

parser!{
  fn string_literal[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    parser(|i| {
      let pos = i.position();
      let start = one_of(b"\"'%/`"[..]).parse(i)?;
      let (kind, is_list, term, does_interp) = match start {
        b'%' => {
          let mut tag = one_of(b"iqrswxIQRSWX"[..]).or(none_of(alpha_num())).parse_stream(i)?;
          let sep = get_lit_end(if ::ascii::AsciiChar::from(tag).is_alphabetic() {
            tag = 'Q';
            none_of(alpha_num()).parse_stream(i)
          } else { tag });

          match tag {
            b'i' => (Kind::SYM, true, sep, false),
            b'I' => (Kind::SYM, true, sep, true),
            b's' => (Kind::SYM, false, sep, false),
            b'S' => (Kind::SYM, false, sep, true),
            b'q' => (Kind::STR, false, sep, false),
            b'Q' => (Kind::STR, false, sep, true),
            b'w' => (Kind::STR, true, sep, false),
            b'W' => (Kind::STR, true, sep, true),
            b'r' => (Kind::REGX, false, sep, false),
            b'R' => (Kind::REGX, false, sep, true),
            b'x' => (Kind::XSTR, false, sep, false),
            b'X' => (Kind::XSTR, false, sep, true),
          }
        },
        b'\'' => (Kind::STR, false, b'\'', false),
        b'"' => (Kind::STR, false, b'"', true),
        b'/' => (Kind::REGX, false, b'/', true),
        b'`' => (Kind::XSTR, false, b'`', true),
        _ => panic!(),
      };

      let sep_match =
        if does_interp { none_of(range([term, b'#'])) } else { none_of(token(term))};
      let str_part_char = range(['\\', term]).map(|_| term).or(
        if is_list {
          choice![range(('\\', one_of(spaces[..].or(nl)).map(|(_, v)| v))), sep_match]
        } else { sep_match });
      let str_part = if does_interp {
        choice![
          (range(&b"#{"[..]), compstmt, token(b'}')).map(|(_, s, _)| s),
          many1(range(&b"\\#"[..]).map(|_| b'#').or(str_part_char))]
      } else {
        many1(str_part_char)
      };

      if is_list {
        nd!(ARRAY; sep_by(str_part, spaces[..].or(nl)).parse_stream(i)?)
      } else {
        nd!(kind; many(str_part).parse_stream(i)?)
      }
    })
  }
}

parser!{
  fn string[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice![
      (token(b'?'), any).map(|(_, v)| v),
      (token('\''),
       many(range(&b"\\'"[..]).map(|_| '\'').or(none_of(token('\'')))),
       token('\'')),
      (token('"'),
       many(choice![
         many(choice![
           range(&b"\\#"[..]).map(|_| '#'),
           range(&b"\\'"[..]).map(|_| '\''),
           none_of(range(&b"'#"[..]))
         ]).map(|v| Node::new_sym(Kind::SYM, Symbol::from(v))),
         (range(&b"#{"[..]), compstmt, token(b'}')).map(|(_, s, _)| s)
       ]),
       token('"'))]
  }
}

parser!{
  fn symbol[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice![
      (basic_symbol).map(|v| nd!(SYM, v)),
      (token(':'), string())]
  }
}

parser!{
  fn basic_symbol[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    (token(b':'), choice![fname, ivar, gvar, cvar]).map(|(_, v)| v)
  }
}

parser!{
  fn integer[I]()(I) -> i32 where [I: Stream<Item = u8>] {
    choice![
      (token('0'),
       choice![
         (one_of("xX".bytes()), many1(one_of(hex_digit().or(token('_'))))),
         (one_of("bB".bytes()), many1(one_of("01_".bytes()))),
         (one_of("dD".bytes()), many1(digit().or(token('_'))))
       ]),
      (one_of("123456789".bytes()), many(digit().or(token('_'))))
    ]
  }
}

parser!{
  fn float_token[I]()(I) -> Vec<u8> where [I: Stream<Item = u8>] {
    let num = many(one_of("0123456789_".bytes()));
    (num, token(b'.'), num, optional((one_of(b"eE".bytes()), one_of("-+".bytes()), num)))
  }
}

parser!{
  fn numeric[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice!(
      float_token,
      integer(),
      (token(b'-'), choice!(integer, float_token)).map(|(_, v)| nd!(NEGATE, v)))
  }
}

parser!{
  fn variable[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice!(ident(), ivar(), gvar(), cvar(), constant()).map(|v| nd!(LVAR, v))
  }
}

parser!{
  fn var_lhs[I]()(I) -> NodeRef where [I: Stream<Item = u8>] { variable() }
}

parser!{
  fn var_ref[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    choice!(
      variable(),
      kw!(nil).map(|_| nd!(NIL)),
      kw!(self).map(|_| nd!(SELF)),
      kw!(true).map(|_| nd!(TRUE)),
      kw!(false).map(|_| nd!(FALSE)),
      range(&b"__FILE__"[..]).map(|_| nd!(STR)),
      (position(), range(&b"__LINE__"[..])).map(|(pos, _)| nd!(INT, pos.line)))
  }
}

parser!{
  fn back_ref[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    (token('$'), one_of(b"&`'+".iter())).map(|(_, c)| Symbol::from(['$', c]))
  }
}

parser!{
  fn nth_ref[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    (token('$'), one_of(b"123456789".iter())).map(|(_, c)| Symbol::from(['$', c]))
  }
}

parser!{
  fn backref[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    choice![nth_ref(), back_ref()]
  }
}

parser!{
  fn superclass[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (token(b'<'), opt_ws_nl(), expr, term).map(|(_, _, e, _)| e)
  }
}

parser!{
  fn farglist[I]()(I) -> Vec<NodeRef> where [I: Stream<Item = u8>] {
    (token(b'('), opt_ws_nl(), fargs, opt_ws_nl(), rparen).map(|(_, _, v, _, _)| v)
      .or((fargs, term).map(|(_, v)| v))
  }
}

parser!{
  fn fargs[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (many((f_arg_item, opt_ws, token(b','))), many((f_opt, opt_ws, ",")),
     f_restarg, many1((token(b','), f_arg_item)), many((token(b','), f_opt)),
     optional((token(b','), f_blockarg)))
      .map(|(pre, o, r, post, post_opt, b)| nd!(ARGS, pre, o, r, post, post_opt, b))
      .or((many(f_arg_item, opt_ws, token(b',')), sep_by1(f_opt, token(b',')),
           optional((token(b','), opt_ws_nl(), f_blockarg)))
          .map(|pre, o, b| nd!(ARGS, pre, o, None, vec![], b)))
      .or((sep_by1((f_arg_item, token(b','))), optional(token(b','), f_blockarg))
          .map(|pre, b| nd!(ARGS, pre, vec![], None, vec![], b)))
      .or((optional(f_blockarg)).map(|b| nd!(ARGS, vec![], vec![], None, vec![], b)))
  }
}

parser!{
  fn block_param[I]()(I) -> Vec<NodeRef> where [I: Stream<Item = u8>] {
    (many((f_arg_item, opt_ws, token(b','))), many((b_opt, opt_ws, ",")),
     f_restarg, many1((token(b','), f_arg_item)), many((token(b','), b_opt)),
     optional((token(b','), f_blockarg)))
      .map(|(pre, o, r, post, post_o, b)| nd!(ARGS, pre, o, r, post, post_o, b))
      .or((many(f_arg_item, opt_ws, token(b',')), sep_by1(b_opt, token(b',')),
           optional((token(b','), opt_ws_nl(), f_blockarg)))
          .map(|(pre, o, b)| {
            nd!(ARGS, pre, o, None, vec![], b)
          }))
      .or((sep_by1((f_arg_item, token(b','))), optional(token(b','), f_blockarg))
          .map(|(pre, b)| {
            nd!(ARGS, pre, vec![], None, vec![], b)
          }))
      .or((optional(f_blockarg)).map(|b| nd!(ARGS, vec![], vec![], None, vec![], b)))
  }
}

parser!{
  fn f_arg_item[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (ident).map(|i| nd!(ARG, i))
      .or((token(b'('), opt_ws_nl(), f_margs, opt_ws_nl(), token(b')'))
          .map(|(_, _, a, _, _)| nd!(MASGN, a, None)))
  }
}

parser!{
  fn f_opt[I]()(I) -> (Symbol, NodeRef) where [I: Stream<Item = u8>] {
    (ident(), opt_ws(), token(b'='), opt_ws_nl(), arg()).map(|(o, _, _, _, a)| (o, a))
  }
}

parser!{
  fn b_opt[I]()(I) -> (Symbol, NodeRef) where [I: Stream<Item = u8>] {
    (ident(), opt_ws(), token(b'='), opt_ws_nl(), primary()).map(|(o, _, _, _, a)| (o, a))
  }
}

parser!{
  fn f_restarg[I]()(I) -> OptNodeRef where [I: Stream<Item = u8>] {
    (token(b'*'), opt_ws_nl(), optional(ident)).map(|(_, _, name)| name)
  }
}

parser!{
  fn f_blockarg[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (token(b'&'), opt_ws_nl(), ident).map(|(_, _, v)| v)
  }
}

parser!{
  fn singleton[I]()(I) -> NodeRef where [I: Stream<Item = u8>] {
    (var_ref)
      .or((token(b'{'), expr, rparen).map(|(_, e, _)| e))
  }
}

parser!{
  fn assoc[I]()(I) -> Vec<(NodeRef, NodeRef)> where [I: Stream<Item = u8>] {
    choice![
      (arg, opt_ws_nl(), range(&b"=>"[..]), opt_ws_nl(), arg).map(|(k, _, _, _, v)| (k, v)),
      (ident, token(':'), opt_ws_nl(), arg).map(|(k, _, _, v)| (nd!(SYM, k), v))]
  }
}

parser!{
  fn operation[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    choice![ident(), constant(), fid()]
  }
}

parser!{
  fn operation2[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    choice![ident(), constant(), fid(), op()]
  }
}

parser!{
  fn operation3[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    choice![ident(), fid(), op()]
  }
}

parser!{
  fn dot_or_colon[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    range(&b"."[..]).map(|v| Symbol::from(v.to_string()))
      .or(range(&b"::"[..]).map(|v| Symbol::from(v.to_string())))
  }
}

parser!{
  fn call_op[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    (opt_ws_nl(),
     choice!(range(&b"."), range(&b"&."[..])).map(|v| Symbol::from(v.to_string())),
     opt_ws_nl()
    )
  }
}

parser!{
  fn call_op2[I]()(I) -> Symbol where [I: Stream<Item = u8>] {
    choice![(opt_ws_nl(), range(&b"::"[..]).map(|v| Symbol::from(v.to_string())), opt_ws_nl()).map(|(_, v, _)| v),
            call_op()]
  }
}

parser!{
  fn rparen[I]()(I) -> () where [I: Stream<Item = u8>] {
    (opt_ws_nl(), token(b')')).map(|_| ())
  }
}

parser!{
  fn rbracket[I]()(I) -> () where [I: Stream<Item = u8>] {
    (opt_ws_nl(), token(b']')).map(|_| ())
  }
}

parser!{
  fn term[I]()(I) -> () where [I: Stream<Item = u8>] {
    choice![
      (opt_ws(), token(b';'), opt_ws()).map(|_| ()),
      nl()
    ]
  }
}

parser!{
  fn comment[I]()(I) -> () where [I: Stream<Item = u8>] {
    (opt_ws(), token(b'#'), many(choice!(range(&b"\\\n"[..]), none_of("\n".chars()))), token('\n'))
      .map(|(_, _, com, _)| ())
  }
}

parser!{
  fn nl[I]()(I) -> () where [I: Stream<Item = u8>] {
    choice!(
      (opt_ws(), token('\n'), opt_ws()).map(|(_, _, _)| ()),
      comment()
    )
  }
}
