use ast::NodeRef;
use ast::OptNodeRef;
use ast::CpathType;
use lexer::Lexer;
use lexer::LexerState;
use lexer::Tok;
use combine::*;
use combine::range::range;

pub fn program<I>(i: I) -> ParseResult<NodeRef, I>
  where I: Stream<Item = char>
{ top_stmts(i) }

fn str2symbol(v: &str) -> Symbol { Symbol::from(v) }

fn ws<T>(i: I) -> ParseResult<(), I> where I: Stream<Item = char> {
  many1(one_of(" \t\x0c\r\x5c")).parse_stream(i)
}
fn opt_ws<T>(i: I) -> ParseResult<(), I> where I: Stream<Item = char> {
  many(one_of(" \t\x0c\r\x5c")).parse_stream(i)
}

fn ws_nl<T>(i: I) -> ParseResult<(), I> where I: Stream<Item = char> {
  many1(one_of(" \t\x0c\r\x5c").or(nl)).parse_stream(i)
}

fn opt_ws_nl<T>(i: I) -> ParseResult<(), I> where I: Stream<Item = char> {
  many(one_of(" \t\x0c\r\x5c").or(nl)).parse_stream(i)
}

fn top_stmts<T>(i: I) -> ParseResult<Vec<NodeRef>, I> where I: Stream<Item = char>
{
  sep_end_by(stmt, term)
    .parse_stream(i)
}

fn top_stmt<I>(i: I) -> ParseResult<Vec<NodeRef>, I> where I: Stream<Item = char> {
  stmt
    .or((string("BEGIN"), opt_ws, token('{'),
         opt_ws, top_stmts,
         opt_ws, tokens('}'))
        .map(|_, _, _, _, stmts, _, _| stmts ))
    .parse_stream(i)
}

fn bodystmt<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (compstmt, optional(rescue), optional(else_),
   optional(string("ensure"), compstmt))
    .map(|c, r, el, en| p.new_bodystmt(c, r, el, en))
    .parse_stream(i)
}

fn compstmt<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  stmts.parse_stream(i)
}

fn stmts<I>(i: I) -> ParseResult<vec<NodeRef>, I> where I: Stream<Item = char> {
  sep_end_by(stmt, term).parse_stream(i)
}

fn assign_op<I>(i: I) -> ParseResult<(), I> where I: Stream<Item = char> {
  (opt_ws_nl, token('='), opt_ws_nl)
    .map(|_, _, _| ())
    .parse_stream(i)
}

fn stmt_no_mod<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (string(&b"alias"[..]), alias_fsym, fsym).map(|_, a, f| new_alias(a, f))
    .or((string("undef"), undef_list).map(|_, l| new_undef(l)))
    .or(command_asgn)
    .or((string("END"), opt_ws, token('{'),
         opt_ws_nl, compstmt, opt_ws_nl,
         token('}')).map(|_, _, _, _, stmt, _, _| stmt))
    .or((mlhs, assign_op, command_call)
        .map(|l, _, c| new_masgn(l, c)))
    .or((lhs, assign_op, mrhs)
        .map(|l, _, r| new_array(r)))
    .or((mlhs, assign_op, arg)
        .map(|l, _, a| new_masgn(l, a)))
    .or((mlhs, assign_op, mrhs)
        .map(|l, _, r| new_masgn(l, new_array(r))))
    .or(expr)
    .parse_stream(i)
}

fn stmt<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (stmt_no_mod, ws, string("if"), opt_ws_nl,
   stmt_no_mod).map(|l, _, _, _, r| new_unless(cond(r), s, None))
    .or((stmt_no_mod, ws, string("unless"), opt_ws_nl,
         stmt_no_mod).map(|l, _, _, _, r| new_unless(cond(r), s, None)))
    .or((stmt_no_mod, ws, string("while"), opt_ws_nl,
         stmt_no_mod).map(|l, _, _, _, r| new_unless(cond(r), s, None)))
    .or((stmt_no_mod, ws, string("until"), opt_ws_nl,
         stmt_no_mod).map(|l, _, _, _, r| new_unless(cond(r), s, None)))
    .or(stmt_no_mod)
    .parse_stream(i)
}

fn op_asgn<I>(i: I) -> ParseResult<Symbol, I> where I: Stream<Item = char> {
  (opt_ws,
   string("*=")
   .or(string("/="))
   .or(string("+="))
   .or(string("-="))
   .or(string("%="))
   .or(string("<<="))
   .or(string(">>="))
   .or(string("&="))
   .or(string("|="))
   .or(string("^="))
   .map(|v| Symbol::from(v.get(..(v.len() - 1)))),
   opt_ws_nl
  ).map(|_, v, _| v).parse_stream(i)
}

fn command_asgn<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (lhs, assign_op, command_rhs).map(|l, _, r| p.new_asgn(l, r))
    .or((varlhs, op_asgn, command_rhs).map(|l, op, r| p.new_op_asgn(l, op, r)))
    .or((primary, opt_ws, token('['), opt_ws_nl,
         opt_call_args, parser_(opt_ws_nl), rbracket,
         opt_ws, op_asgn, opt_ws_nl, command_rhs)
        .map(|prim, _, _, _, idx, _, _, _, op, _, r|
             new_op_asgn(new_call(prim, Symbol::from("[]"), idx, Symbol::from(".")), op, r)))
    .or((primary, call_op, ident, op_asgn, command_rhs)
        .map(|prim, c, id, op, r| new_op_asgn(new_call(prim, id, vec![], c), op, r)))
    .or((primary, call_op, constant, op_asgn, command_rhs)
        .map(|prim, c, id, op, r| new_op_asgn(new_call(prim, id, vec![], c), op, r)))
    .or((primary, opt_ws, "::", opt_ws, ident, op_asgn, command_rhs)
        .map(|prim, _, _, _, id, op, r|
             new_op_asgn(new_call(prim, id, vec![], Symbol::from("::"), op, r))))
    .parse_stream(i)
}

fn command_rhs<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(command_call).or(command_asgn).parse_stream(i)
}

fn expr_not<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (string("not"), ws_nl, arg).map(|_, _, e| call_uni_op(cond(e), "!"))
    .or(arg)
    .parse_stream(i)
}

fn expr_and_or<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  chainl1(expr_not,
          (opt_ws,
           string("and").map(|_| |l: NodeRef, r: NodeRef| new_and(l, r))
           .or(string("or").map(|_| |l: NodeRef, r: NodeRef| new_or(l, r))),
           opt_ws_nl).map(|_, v, _| v))
    .parse_stream(i)
}

fn expr<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(command_call)
    .or(command_call)
    .or(expr_and_or)
    .or((string("!"), opt_ws_nl, commandcall).map(|_, _, c| call_uni_op(cond(c), "!")))
    .parse_stream(i)
}

fn command_call<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(command).or(block_command).parse_stream(i)
}

fn BlockCommand<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(block_call)
    .or((block_call, call_op2, operation2, ws, command_args)
        .map(|b, c, o, _, args| p.new_call(b, c, o, args)))
    .parse_stream(i)
}

fn Command<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (operation, wp, command_args).map(|op, _, c| new_fcall(op, c))
    .or((primary, call_op, operation2, ws, command_args)
        .map(|prim, c_op, op, _, c| new_call(prim, op, c, c_op)))
    .or((primary, opt_ws, "::", opt_ws_nl, operation2, command_args)
        .map(|prim, _, _, _, op, c| new_call(prim, op, c, Symbol::from("::"))))
    .or((string("super"), ws_nl, command_args).map(|_, _, c| new_super(c)))
    .or((string("yieldr"), ws_nl, command_args).map(|_, _, c| new_yield(c)))
    .or((string("super"), ws_nl, command_args).map(|_, _, c| new_super(c)))
    .or((string("return"), ws_nl, call_args).map(|_, _, c| new_return(ret_args(c))))
    .or((string("break"), ws_nl, call_args).map(|_, _, c| new_break(ret_args(c))))
    .or((string("next"), ws_nl, call_args).map(|_, _, c| new_next(ret_args(c))))
}

fn mlhs<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(mlhs_basic)
    .or((token('('), mlhs_inner, rparen).map(|_, mlhs, _| mlhs))
    .parse_stream(i)
}

fn mlhs_inner<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(mlhs_basic)
    .or((token('('), mlhs_inner, rparen).map(|_, mlhs, _| mlhs))
    .parse_stream(i)
}

fn mlhs_basic<I>(i: I) -> ParseResult<Vec<NodeRef>, I> where I: Stream<Item = char> {
  (many((mlhs_item, opt_ws, token(','), opt_ws_nl).map(|v, _, _, _| v)),
   opt_ws_nl, token('*'), opt_ws, optional(lhs),
   many((opt_ws, token(','), opt_ws_nl, mlhs_item).map(|_, _, _, v| v))
   .map(|pre, _, _, _, ary, post| { pre.push(ary); pre.concat(post) }))
    .or(sep_by(mlhs_item, (opt_ws, token(','), opt_ws_nl)))
    .parse_stream(i)
}

fn mlhs_item<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(lhs)
    .or((token('('), opt_ws, mlhs_inner, rparen).map(|_, _, v, _| new_masgn(v, vec![])))
    .parse_stream(i)
}

fn lhs<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  variable.map(|v| assignable(v))
    .or((primary, opt_ws, token('['), opt_ws, opt_call_args, ws, rbracket)
        .map(|prim, _, _, c, _, _| p.new_call(prim, Symbol::from("[]"), c, Symbol::from("."))))
    .or((primary, opt_ws_nl, call_op, opt_ws_nl, ident)
        .map(|prim, _, c, _, id| p.new_call(prim, id, vec![], c)))
    .or((primary, opt_ws, string("::"), opt_ws_nl, ident)
        .map(|prim, _, _, _, id| p.new_call(prim, id, vec![], Symbol::from("::"))))
    .or((primary, opt_ws_nl, call_op, opt_ws_nl, constant)
        .map(|prim, _, c, _, id| p.new_call(prim, id, vec![], c)))
    .or((primary, opt_ws_nl, string("::"), opt_ws_nl, constant)
        .map(|prim, _, c, _, id| p.new_call(prim, id, vec![], Symbol::from("::"))))
    .or((string("::"), opt_ws_nl, constant)
        .map(|_, _, id| p.new_colon3(id)))
    .parse_stream(i)
}

fn cname<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  constant.parse_stream(i)
}

fn cpath<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (string("::"), opt_ws_nl, cname).map(|_, _, name| new_absolute_name(name))
    .or(cname)
    .or((primary, opt_ws, string("::"), opt_ws_nl, cname)
        .map(|prim, _, _, _, name| new_const_ref(prim, name)))
    .parse_stream(i)
}

fn fname<I>(i: I) -> ParseResult<Symbol, I> where I: Stream<Item = char> {
  parser(ident).or(constant).or(fid).or(op).or(reswords)
    .parse_stream(i)
}

fn fsym<I>(i: I) -> ParseResult<Symbol, I> where I: Stream<Item = char> {
  parser(fname).or(basic_symbol)
    .parser_stream(i)
}

fn UndefList<I>(i: I) -> ParseResult<Vec<Symbol>, I> where I: Stream<Item = char> {
  sep_by(fsym, (opt_ws, token(','), opt_ws_nl))
    .parse_stream(i)
}

fn op<I>(i: I) -> ParseResult<Symbol, I> where I: Stream<Item = char> {
  string("|")
    .or(string("^"))
    .or(string("&"))
    .or(string("<=>"))
    .or(string("=="))
    .or(string("==="))
    .or(string("=~"))
    .or(string("!~"))
    .or(string(">"))
    .or(string(">="))
    .or(string("<"))
    .or(string("<="))
    .or(string("!="))
    .or(string("<<"))
    .or(string(">>"))
    .or(string("+"))
    .or(string("-"))
    .or(string("*"))
    .or(string("/"))
    .or(string("%"))
    .or(string("**"))
    .or(string("!"))
    .or(string("~"))
    .or(string("+@"))
    .or(string("-@"))
    .or(string("[]"))
    .or(string("[]="))
    .or(string("`"))
    .map(str2symbol).parse_stream(i);
}

fn reswords<I>(i: I) -> ParseResult<Symbol, I> where I: Stream<Item = char> {
  string("__LINE__")
    .or(string("__FILE__"))
    .or(string("__ENCODING__"))
    .or(string("BEGIN"))
    .or(string("END"))
    .or(string("alias"))
    .or(string("and"))
    .or(string("begin"))
    .or(string("break"))
    .or(string("case"))
    .or(string("class"))
    .or(string("def"))
    .or(string("do"))
    .or(string("else"))
    .or(string("elsif"))
    .or(string("end"))
    .or(string("ensure"))
    .or(string("false"))
    .or(string("for"))
    .or(string("in"))
    .or(string("module"))
    .or(string("next"))
    .or(string("nil"))
    .or(string("not"))
    .or(string("or"))
    .or(string("redo"))
    .or(string("rescue"))
    .or(string("retry"))
    .or(string("return"))
    .or(string("self"))
    .or(string("super"))
    .or(string("then"))
    .or(string("true"))
    .or(string("undef"))
    .or(string("when"))
    .or(string("yield"))
    .or(string("if"))
    .or(string("unless"))
    .or(string("while"))
    .or(string("until"))
    .map(|v| Symbol::from(v)).parse_stream(i)
}

fn arg1<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (many((token('!').or(token('~')).or(token('+')), opt_ws_nl).map(|op, _| op)),
   primary)
    .map(|op, _| op)
    .parse_stream(i)
}

fn arg2<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  chainr1(arg1,
          string("**").map(|op| |l: NodeRef, r: NodeRef| call_bin_op(l, "**", r)))
    .parse_stream(i)
}

fn arg3<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (many((token('-'), opt_ws_nl).map(|op, _| op)), arg2)
    .map(|op, a| call_uni_op(a, "-"))
    .parse_stream(i)
}

fn arg4<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  chainl1(arg3, (opt_ws, token('*').or(token('/')).or(token('%')), opt_ws_nl))
    .parse_stream(i)
}

fn arg5<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  chainl1(arg4, (opt_ws, token('+').or(token('-')), opt_ws_nl))
    .parse_stream(i)
}

fn arg6<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  chainl1(arg5, (opt_ws, string("<<").or(string(">>")), opt_ws_nl))
    .parse_stream(i)
}

fn arg7<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  chainl1(arg6, (opt_ws, token('&'), opt_ws_nl))
    .parse_stream(i)
}

fn arg8<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  chainl1(arg7, (opt_ws, token('|').or(token('~')), opt_ws_nl))
    .parse_stream(i)
}

fn arg9<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  chainl1(arg8, (opt_ws, string(">").or(string(">=")).or(string("<")).or(string("<=")), opt_ws_nl))
    .parse_stream(i)
}

fn arg10<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  chainl1(arg9, (opt_ws,
                 string("<=>")
                 .or(string("=="))
                 .or(string("==="))
                 .or(string("!="))
                 .or(string("=~"))
                 .or(string("!~")),
                 opt_ws_nl))
    .parse_stream(i)
}

fn arg11<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  chainl1(arg10, (opt_ws, string("&&"), opt_ws_nl))
    .parse_stream(i)
}

fn arg12<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  chainl1(arg11, (opt_ws, string("||"), opt_ws_nl))
    .parse_stream(i)
}

fn arg13<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  chainl1(arg12, (opt_ws, string("..").or(string("...")), opt_ws_nl))
    .parse_stream(i)
}

fn arg14<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (arg13, opt_ws, token('?'), opt_ws_nl, arg, opt_ws_nl, token(':'), opt_ws_nl, arg)
    .map(|c, _, _, _, t, _, _, _, f| p.new_if(p.cond(c), t, f))
    .or(arg13)
}

fn arg_rhs<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(arg14)
    .or((arg_rhs, ws, string("rescue"), ws_nl, arg14)
        .map(|a, _, _, _, r| new_mod_rescue(a, r)))
    .parse_stream(i)
}

fn arg<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (lhs, opt_ws, token('='), opt_ws_nl, arg_rhs).map(|l, _, _, _, r| new_asgn(l, r))
    .or((var_lhs, opt_ws, op_asgn, opt_ws_nl, arg_rhs).map(|l, _, op, _, r| new_op_asgn(l, op, r)))
    .or((primary, opt_ws, token('['), opt_ws_nl, opt_call_args, opt_ws_nl, rbracket,
         opt_ws, op_asgn, opt_ws_nl, arg_rhs)
        .map(|prim, _, _, _, args, _, _, _, op, r|
             new_op_asgn(new_call(prim, Symbol::from("[]"), args), op, r)))
    .or((primary, opt_ws_nl, call_op, opt_ws_nl, ident, opt_ws, op_asgn, opt_ws_nl, arg_rhs)
        .map(|prim, _, c_op, _, id, _, op, _, r| new_op_asgn(new_call(prim, id, vec![], c_op), op, r)))
    .or((primary, opt_ws_nl, call_op, opt_ws_nl, constant, opt_ws, op_asgn, opt_ws_nl, arg_rhs)
        .map(|prim, _, c_op, _, id, _, op, _, r| new_op_asgn(new_call(prim, id, vec![], c_op), op, r)))
    .or((primary, opt_ws_nl, string("::"), opt_ws_nl, ident, opt_ws, op_asgn, opt_ws_nl, arg_rhs)
        .map(|prim, _, _, _, id, _, op, _, r|
             new_op_asgn(new_call(prim, id, vec![], Symbol::from("::")), op, r)))
    .or(arg14)
    .parse_stream(i)
}


fn Trailer<I>(i: I) -> ParseResult<(), I> where I: Stream<Item = char> {
  (opt_ws_nl)
    .or((opt_ws, token(','), opt_ws))
    .parse_stream(i)
}
fn aref_args<I>(i: I) -> ParseResult<Vec<NodeRef>, I> where I: Stream<Item = char> {
  (sep_by1(arg, comma_hd),
   optional((comma_hd, "*", opt_ws_nl, arg).map(|_, _, _, a| a)),
   optional((comma_hd, assoc).map(|_, h| h)),
   optional(comma_hd))
    .map(|args, sp, h, _| {
      match sp { Some(sp) => { args.push(new_splat(sp)); }, None => {} };
      match h { Some(h) => { args.push(new_hash(h)); }, None => {} };
      args
    })
    .or((
      ("*", opt_ws_nl, arg).map(|_, _, a| a),
      optional((comma_hd, assoc).map(|_, h| h)),
      optional(comma_hd))
        .map(|sp, h, _| {
          let args = vec![sp];
          match h { Some(h) => { args.push(new_hash(h)); }, None => {} };
          args
        }))
    .or((
      assoc.map(|_, h| h),
      optional(comma_hd))
        .map(|h, _| vec![new_hash(h)]))
    .or(opt_ws_nl.map(|_| vec![]))
}

fn paren_args<I>(i: I) -> ParseResult<Vec<NodeRef>, I> where I: Stream<Item = char> {
  (token('('), opt_ws_nl, opt_call_args, rparen)
    .map(|_, _, a, _| a)
    .parse_stream(i)
}

fn opt_callargs<I>(i: I) -> ParseResult<Vec<NodeRef>, I> where I: Stream<Item = char> {
  (callargs, optional(comma_hd))
    .map(|args, _| args)
    .parse_stream(i)
}

fn callargs<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(command).map(|v| vec![v])
    .or((sep_by1(arg, comma_hd),
         optiona((comma_hd, token('*'), arg).map(|_, _, sp| new_splat(sp))),
         many((comma_hd, arg).map(|_, a| a)),
         optional((comma_hd, assoc).map(|_, h| new_hash(h))),
         optional((comma_hd, blockarg).map(|_, b| b)))
        .map(|args, sp, post, h, b| {
          match sp { Some(sp) => { args.push(sp); }, None => {} };
          args.append(post);
          match h { Some(h) => { args.push(h); }, None => {} };
          match b { Some(b) => { args.push(b); }, None => {} };
          args
        }))
    .or(((token('*'), arg).map(|_, sp| new_splat(sp)),
         many((comma_hd, arg).map(|_, a| a)),
         optional((comma_hd, assoc).map(|_, h| new_hash(h))),
         optional((comma_hd, blockarg).map(|_, b| b)))
        .map(|sp, args, h, b| {
          match sp { Some(sp) => { args.insert(0, sp); }, None => {} };
          match h { Some(h) => { args.push(h); }, None => {} };
          match b { Some(b) => { args.push(b); }, None => {} };
          args
        }))
    .or((assoc.map(|_, h| h),
         optional((comma_hd, blockarg).map(|_, b| b)))
        .map(|h, b| {
          let mut args = vec![];
          match h { Some(h) => { args.push(new_hash(h)); }, None => {} };
          match b { Some(b) => { args.push(b); }, None => {} };
          args
        }))
    .or((comma_hd, blockarg).map(|_, b| vec![b]))
}

fn command_args<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  call_args.parse_stream(i)
}

fn blockarg<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (token('&'), opt_ws_nl, arg).map(|_, _, a| new_block_arg(a))
    .parse_stream(i)
}

fn comma_hd<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (opt_ws, token(","), opt_ws_nl, many(heredoc_body)).parse_stream(i)
}

fn mrhs<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  sep_by1(arg, comma)
    .or((sep_end_by(arg, comma),
         token("*"), opt_ws, arg).map(|args, _, _, sp| {
           args.push(new_splat(sp));
           args
         }))
    .parse_stream(i)
}

fn primary<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(literal)
    .or(string)
    .or(xstring)
    .or(regexp)
    .or(heredoc)
    .or(var_ref)
    .or(backref)
    .or(fid.map(|v| new_fcall(v, vec![])))
    .or(between((string("begin"), ws_nl), (opt_ws_nl, string("end")), bodystmt))
    .or(between((token('('), opt_ws_nl), (opt_ws_nl, token(')')), stmt))
    .or((token('('), opt_ws_nl, token(')')).map(|_, _, | new_nil()))
    .or(between((token('('), opt_ws_nl), (opt_ws_nl, token(')')), compstmt))
    .or((primary, opt_ws, string("::"), opt_ws_nl, constant).map(|prim, _, _, _, id| new_colon2(prim, id)))
    .or((string("::"), opt_ws_nl, constant).map(|_, _, id| new_colon3(id)))
    .or(between((token('['), opt_ws_nl), (opt_ws_nl, token(']')),
                aref_args.map(|v| new_array(v))))
    .or(string("return").map(|_| new_return(vec![])))
    .or((string("yield"), ws, paren_args).map(|_, _, args| new_yield(args)))
    .or((string("not"), ws, between((token('('), opt_ws_nl), (opt_ws_nl, token(')')),
                                    optional(expr)))
        .map(|_, _, e| p.call_uni_op(match e { Some(e) => p.cond(e), None => new_nil() },
                                     Symbol::from("!"))))
    .or((operation, opt_ws_nl, braceblock).map(|op, _, b| new_fcall(op, b)))
    .or((method_call, opt_ws_nl, braceblock).map(|op, _, b| call_with_block(op, b)))
    .or(method_call)
    .or((string("->"), opt_ws_nl, flarglist, opt_ws_nl, lambdabody)
        .map(|_, _, a, _, b| new_lambda(a, b)))
    .or((string("if"), opt_ws_nl, expr, opt_ws, then, opt_ws_nl, compstmt, opt_ws_nl,
         if_tail, opt_ws_nl, string("end"))
        .map(|_, _, c, _, _, _, s, _, t| new_if(cond(c), s, t)))
    .or((string("unless"), opt_ws_nl, expr, opt_ws, then, opt_ws_nl, compstmt, opt_ws_nl,
         else_, opt_ws_nl, string("end"))
        .map(|_, _, c, _, _, _, s, _, t| new_unless(cond(c), s, t)))
    .or((string("while"), opt_ws_nl, expr, opt_ws, do_, opt_ws_nl, compstmt, opt_ws_nl, string("end"))
        .map(|_, _, c, _, _, _, s, _, t| new_while(cond(c), s, t)))
    .or((string("until"), opt_ws_nl, expr, opt_ws, do_, opt_ws_nl, compstmt, opt_ws_nl, string("end"))
        .map(|_, _, c, _, _, _, s, _, t| new_until(cond(c), s, t)))
    .or((string("case"), opt_ws_nl, expr, many(term), opt_ws_nl, casebody, opt_ws_nl, string("end"))
        .map(|_, _, c, _, _, _, s, _, t| new_case(cond(c), s, t)))
    .or((string("case"), opt_ws_nl, opt_ws_nl, casebody, opt_ws_nl, string("end"))
        .map(|_, _, c, _, _, _, s, _, t| new_case(cond(c), s, t)))
    .or((string("for"), opt_ws_nl, forvar, opt_ws_nl, string("in"), opt_ws_nl, expr,
         opt_ws_nl, do_, opt_ws_nl, compstmt, string("end"))
        .map(|_, _, c, _, _, _, s, _, t| new_for(v, e, s)))
    .or((string("class"), opt_ws_nl, cpath, opt_ws_nl, superclass, many(term),
         bodystmt, opt_ws_nl, string("end")).map(|_, _, c, _, s, _, b, _, _| new_class(c, s, b)))
    .or((string("class"), opt_ws_nl, string("<<"), opt_ws_nl, expr, many(term),
         bodystmt, opt_ws_nl, string("end")).map(|_, _, c, _, s, _, b, _, _| new_sclass(c, b)))
    .or((string("module"), opt_ws_nl, cpath, many(term),
         bodystmt, opt_ws_nl, string("end")).map(|_, _, c, _, s, _, b, _, _| new_sclass(c, b)))
    .or((string("def"), opt_ws_nl, fname, ws, farglist, term, bodystmt, opt_ws_nl, string("end"))
        .map(|_, _, f, _, args, _, b, _, _| new_def(f, a, b)))
    .or(string("break").map(|_| new_break(vec![])))
    .or(string("next").map(|_| new_next(vec![])))
    .or(string("redo").map(|_| new_redo(vec![])))
    .or(string("retry").map(|_| new_retry(vec![])))
    .parse_stream(i)
}

fn then<I>(i: I) -> ParseResult<(), I> where I: Stream<Item = char> {
  (term, optional(string("then")))
    .or(string("then"))
    .parse_stream(i)
}

fn do_<I>(i: I) -> ParseResult<(), I> where I: Stream<Item = char> {
  parser(term)
    .or((ws, string("do"), ws_nl))
    .parse_stream(i)
}

fn if_tail<I>(i: I) -> ParseResult<Vec<NodeRef>, I> where I: Stream<Item = char> {
  (many((string("elsif"), ws, expr, opt_ws, then, opt_ws, compstmt)
        .map(|_, _, cond, _, _, _, st| new_elsif(cond, compstmt))),
   opt_ws_nl,
   optional((string("else"), ws_nl, compstmt).map(|_, _, st| new_else(st))))
    .map(|elifs, _, el| {
      match el { Some(el) => { elifs.push(el); }, None => {} };
      elifs
    })
    .parse_stream(i)
}

fn for_var<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(lhs).or(mlhs).parse_stream(i)
}

fn f_marg<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(ident).map(|v| new_arg(v))
    .or(between((token('('), opt_ws_nl), (opt_ws_nl, token(')')),
                f_margs.map(|v| new_masgn(v, vec![]))))
    .parse_stream(i)
}

fn f_margs<I>(i: I) -> ParseResult<(Vec<NodeRef>, Option<OptNodeRef>, Vec<NodeRef>), I>
  where I: Stream<Item = char>
{
  sep_by1(f_marg, (opt_ws_nl, token(','), opt_ws_nl))
    .map(|v| (v, None, vec![]))
    .or((many((f_marg, opt_ws, token(','), opt_ws_nl).map(|v, _, _, _| v)),
         token('*'), opt_ws_nl, optional(ident), opt_ws,
         many((token(','), opt_ws_nl, f_marg, opt_ws).map(|v, _, _, _| v)))
        .map(|pre, _, _, post, _, post| (pre, Some(post), post)))
    .parse_stream(i)
}

fn block_param_def<I>(i: I) -> ParseResult<(Vec<NodeRef>, Vec<NodeRef>), I> where I: Stream<Item = char> {
  string("||").map(|v| (vec![], vec![]))
    .or((token('|'), opt_ws_nl, blockparam, optional(bvars), opt_ws_nl, token('|')))
    .parse_stream(i)
}

fn bvars<I>(i: I) -> ParseResult<Vec<NodeRef>, I> where I: Stream<Item = char> {
  (opt_ws_nl, token(';'), sep_by1(bvar, (opt_ws_nl, token(','), opt_ws_nl)))
    .map(|_, _, v| v)
    .parse_stream(i)
}

fn bvar<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(ident).map(|v| new_bv(v))
    .parse_stream(i)
}

fn f_larglist<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (token('('), opt_ws_nl, fargs, opt_ws_nl, optional(bvars), opt_ws_nl, token(')'))
    .map(|_, _, v, _, b, _| v)
    .or(fargs)
    .parse_stream(i)
}

fn lambda_body<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (token('{'), opt_ws_nl, compstmt, opt_ws_nl, token('}')).map(|_, _, v, _, _| v)
    .or((string("do"), opt_ws_nl, compstmt, opt_ws_nl, string("end")).map(|_, _, v, _, _| v))
    .parse_stream(i)
}

fn do_block<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (string("do"), opt_ws_nl, optional(block_param_def), opt_ws_nl, compstmt, opt_ws_nl, string("end"))
    .map(|_, _, b, _, v, _, _| v)
    .parse_stream(i)
}

fn block_call<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (command, opt_ws, do_block).map(|c, _, b| call_with_block(c, b))
    .or((block_call, opt_ws_nl, call_op2, opt_ws_nl, operation2, optional(paren_args))
        .map(|b, _, c_op, _, op, a| new_call(b, op, a, c_op)))
    .or((block_call, opt_ws_nl, call_op2, opt_ws_nl, operation2, optional(paren_args),
         opt_ws_nl, braceblock)
        .map(|b, _, c_op, _, op, a, _, br| call_with_block(new_call(b, op, a, c_op))), br)
    .or((block_call, opt_ws_nl, call_op2, opt_ws_nl, operation2, optional(paren_args),
         opt_ws_nl, do_block)
        .map(|b, _, c_op, _, op, a, _, br| call_with_block(new_call(b, op, a, c_op))))
}

fn method_call<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (operation, paren_args).map(|op, a| new_fcall(op, a))
    .or((primary, opt_ws_nl, call_op, opt_ws_nl, operation2, optional(paren_args))
        .map(|prim, _, c_op, _, op, a| new_call(prim, op, a, c_op)))
    .or((primary, opt_ws_nl, string("::"), opt_ws_nl, operation2, paren_args)
        .map(|prim, _, c_op, _, op, a| new_call(prim, op, a, c_op)))
    .or((primary, opt_ws_nl, string("::"), opt_ws_nl, operation3)
        .map(|prim, _, c_op, _, op| new_call(prim, op, vec![], "::")))
    .or((primary, opt_ws_nl, call_op, paren_args)
        .map(|prim, _, c_op, a| new_call(prim, Symbol::from("call"), a, c_op)))
    .or((primary, opt_ws_nl, "::", paren_args)
        .map(|prim, _, c_op, a| new_call(prim, Symbol::from("call"), a, "::")))
    .or((string("super"), opt_ws, paren_args)
        .map(|_, _, a| new_super(a)))
    .or((string("super"))
        .map(|_, _, a| new_zsuper()))
    .or((primary, opt_ws, token('['), opt_ws_nl, opt_callargs, rbracket)
        .map(|prim, _, _, _, a, _| new_call(prim, Symbol::from("[]"), a, ".")))
    .parse_stream(i)
}

fn brace_block<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (token('{'), opt_ws, optional(block_param_def), opt_ws_nl,
   compstmt, opt_ws_nl, token('}').map(|_, _, param, _, s, _, _| new_block(param, s)))
    .or((string("do"), opt_ws, optional(block_param_def), opt_ws_nl,
        compstmt, opt_ws_nl, string("end"))
       .map(|_, _, param, _, s, _, _| new_block(param, s)))
    .parse_stream(i)
}

fn case_body<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (string("when"), opt_ws_nl, sep_by(arg, (opt_ws, token(','), opt_ws_nl)),
   opt_ws, then, opt_ws_nl, compstmt, cases)
    .map(|_, _, l, _, _, s_, c| (l, s, Some(c)))
    .parse_stream(i)
}

fn cases<I>(i: I) -> ParseResult<NodeRef, I> {
  (else_).map(|v| (None, v, None))
    .or(case_body)
    .parse_stream(i)
}

fn rescue<I>(i: I) -> ParseResult<Vec<NodeRef>, I> where I: Stream<Item = char> {
  sep_by1((string("rescue"), ws, exc_list, opt_ws, optional((string("=>"), opt_ws_nl, lhs)),
           opt_ws, then, opt_ws_nl, compstmt)
          .map(|_, _, l, _, v, _, _, s| (l, v, s)), nl)
    .parse_stream(i)
}

fn exc_list<I>(i: I) -> ParseResult<Vec<NodeRef>, I> where I: Stream<Item = char> {
  (arg).map(|v| vec![v])
    .or(mrhs)
    .parse_stream(i)
}

fn literal<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(numeric)
    .or(symbol)
    .or(words)
    .or(symbols)
    .parse_stream(i)
}

fn string<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  parser(char_token)
    .or(string_token)
    .or((string_beg, many(string_interp), string_token)
        .map(|_, r, s| { r.push(s); new_dstr(r) }))
    .parse_stream(i)
}

fn string_interp<I>(i: I) -> ParseResult<Vec<NodeRef>, I> where I: Stream<Item = char> {
  (string_mid).map(|v| vec![v])
    .or((string_part, compstmt, token('}')).map(|p, s, _| vec![p, s]))
    .or((literal_delim).map(|v| vec![new_literal_delim()]))
    .or((hd_literal_delim, heredoc_body).map(|v| vec![new_literal_delim()]))
    .parse_stream(i)
}

fn xstring<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  (xstring_beg, xstring_token).map(|_, v| v)
    .or((xstring_beg, many(string_interp), xstring_token)
        .map(|_, r, s| { r.push(s); new_dxstr(r) }))
    .parse_stream(i)
}

fn regexp<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  regexp_beg <"regexp"> => <>,
  "regexp_beg" many(string_interp) <rgx:"regexp"> => p.new_dregx(r, rgx),
}

fn Heredoc<I>(i: I) -> ParseResult<Heredoc, I> where I: Stream<Item = char> { "heredoc_beg" };

fn HeredocBody<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  HeredocStringInterp* "heredoc_end" => {
    let inf = p.parsing_heredoc_inf();
    inf.doc.push(p.new_str(""));
    p.heredoc_end();
  },
}

fn HeredocStringInterp<I>(i: I) -> ParseResult<HeredocStringInterp, I> where I: Stream<Item = char> {
  <"hd_string_mid"> => {
    let inf = p.parsing_heredoc_inf();
    inf.doc.push(p.new_str(<>));
    p.herdoc_treat_nextline();
  },
  <s:"hd_start_part"> // $<nd>$ = p.lex_strterm; p.lex_strterm = NULL;
    compstmt "}" => {
      let inf = p.parsing_heredoc_inf();
      p.lex_strterm = $<nd>2;
      inf.doc.push(s);
      inf.doc.push(c);
    },
}

fn Words<I>(i: I) -> ParseResult<Words, I> where I: Stream<Item = char> {
  "words_beg" <"string"> => { p.new_words(vec![<>]) },
  "words_beg" many(string_interp) <s:"string"> => {
    r.push(s);  p,new_words(r)
  },
}

fn Symbol<I>(i: I) -> ParseResult<Symbol, I> where I: Stream<Item = char> {
  BasicSymbol => p.new_sym(<>),
  "symbeg" "string_beg" many(string_interp) <s:"string"> => {
    p.lstate = LexerState::END;
    r.push(s);
    p.new_dsym(r)
  },
}

BasicSymbol: Symbol = {
  "symbeg" <Sym> => { p.lstate = LexerState::END; <> },
}

Sym: Symbol = {
  Fname, "ivar", "gvar", "cvar",
  <"string"> => Symbol::from(<>),
  "string_beg" <"string"> => Symbol::from(<>),
}

fn Symbols<I>(i: I) -> ParseResult<Symbols, I> where I: Stream<Item = char> {
  "symbols_beg" <"string"> => p.new_symbols(vec![<>]),
  "symbols_beg" many(string_interp) <s:"string"> => {
    r.push(s);
    p.new_symbols(r)
  },
}

Numeric: NodeRef = {
  "integer",
  "float",
  "uminus_num" <"integer"> // %prec "lowest"
    => p.negate_lit(<>),
  "uminus_num" <"float"> // %prec "lowest"
    => p.negate_lit(<>),
}

fn variable<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  ident.or(ivar).or(gvar).or(cvar).or(constant).map(|v| new_lvar(v))
    .parse_stream(i)
}

fn VarLhs<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> { Variable => p.assignable(<>) };

fn VarRef<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  variable.map(|_| var_reference())
    .or(string("nil").map(|_| new_nil()))
    .or(string("self").map(|_| new_self()))
    .or(string("true").map(|_| new_true()))
    .or(string("false").map(|_| new_false()))
    .or(string("__FILE__").map(|_| new_str(<>)))
    .or((postion(), string("__LINE__")).map(|pos, _| new_int(pos.line)))
    .parse_stream(i)
}

fn Backref<I>(i: I) -> ParseResult<Backref, I> where I: Stream<Item = char> { "nth_ref", "back_ref", };

fn Superclass<I>(i: I) -> ParseResult<Superclass, I> where I: Stream<Item = char> {
  Term => None,
  SuperclassSymbol <Expr> Term => <>,
}

fn SuperclassSymbol<I>(i: I) -> ParseResult<SuperclassSymbol, I> where I: Stream<Item = char> {
  "<" => { p.lstate = LexerState::BEG; p.cmd_start = true; }
}

fn Farglist<I>(i: I) -> ParseResult<Farglist, I> where I: Stream<Item = char> {
  "(" <Fargs> Rparen => {
    p.lstate = LexerState::BEG;
    p.cmd_start = true;
    <>
  },
  <Fargs> Term => <>,
}

Fargs = FargsTemp<FOpt>;
BlockParam = FargsTemp<BOpt>;

FargsTemp<Opt>: NodeRef = {
  <pre:(<FargItem> ",")*> <o:(<Opt> ",")*> frestarg <post:("," <FargItem>)+> <b:("," <FBlockarg>)?> =>
    p.new_args(pre, o, r, post, b),
  <pre:(<FargItem> ",")+> <o:(<Opt> ",")*> <last_o:Opt> <b:("," <FBlockarg>)?> => {
    o.push(last_o);
    p.new_args(pre, o, r, vec![], b)
  },
  <pre:(<FargItem> ",")*> <last_pre:FargItem> <b:("," <FBlockarg>)?> => {
    pre.push(last_pre);
    p.new_args(pre, o, r, vec![], b)
  },
  FBlockarg? => p.new_args(vec![], vec![], None, vec![], <>)
}

fn farg_item<I>(i: I) -> ParseResult<NodeRef, I> where I: Stream<Item = char> {
  ident => p.new_arg(<>),
  "lparen" <FMargs> Rparen => p.new_masgn(<>, None),
}

fn f_opt<I>(i: I) -> ParseResult<(Symbol, NodeRef), I> where I: Stream<Item = char> {
  <o:"ident"> "=" arg => (o, a)
}
fn b_opt<I>(i: I) -> ParseResult<(Symbol, NodeRef), I> where I: Stream<Item = char> {
  <o:"ident"> "=" primary => (o, a)
}

fn FRestarg<I>(i: I) -> ParseResult<OptNodeRef, I> {
  (token('*'), opt_ws_nl, optional(ident)).map(|_, _, name| name)
}

fn f_blockarg<I>(i: I) -> ParseResult<NodeRef, I> {
  token('&') <"ident"> => { p.local_add_f(<>); <> }
}

fn singleton<I>(i: I) -> ParseResult<NodeRef, I> {
  VarRef,
  toekn('{') <Expr> Rparen => match <>.node_type() => {
    NodeType::STR | NodeType::DXSTR | NodeType::XSTR | NodeType::DXSTR |
    NodeType::DREGX | NodeType::MATCH | NodeType::FLOAT | NodeType::ARRAY |
    NodeType::HEREDOR =>
      Err("Can't define singleton method of literals."),
    _ => <>,
  },
}

fn assoc<I>(i: I) -> ParseResult<NodeRef, I> {
  arg "=>" arg => {
    p.void_expr_error(k);
    p.void_expr_error(v);
    (k, v)
  },
  label arg => {
    p.void_expr_error(v);
    (p.new_sym(k), v)
  },
  <k:label_end> arg => {
    p.void_expr_error(v);
    (p.new_sym(k), v)
  },
  string_beg many(string_interp) <l:label_end> arg => {
    p.void_expr_error(v);
    s.push(l);
    (p.new_dsym(s), v)
  },
}

fn operation<I>(i: I) -> ParseResult<Symbol, I> where I: Stream<Item = char> {
  ident.or(constant).or(fid).parse_stream(i)
}
fn operation2<I>(i: I) -> ParseResult<Symbol, I> where I: Stream<Item = char> {
  ident.or(constant).or(fid).or(op).parse_stream(i)
}
fn operation3<I>(i: I) -> ParseResult<Symbol, I> where I: Stream<Item = char> {
  ident.or(fid).or(op).parse_stream(i)
}

fn dot_or_colon<I>(i: I) -> ParseResult<Symbol, I> where I: Stream<Item = char> {
  string(".").map(str2symbol)
    .or(string("::").map(str2symbol))
    .parse_stream(i)
}

fn call_op<I>(i: I) -> ParseResult<Symbol, I> where I: Stream<Item = char> {
  (opt_ws_nl,
   string(".").map(str2symbol)
   .or(string("&.").map(str2symbol)),
   opt_ws_nl
  ).parse_stream(i)
}

fn call_op2<I>(i: I) -> ParseResult<Symbol, I> where I: Stream<Item = char> {
  (opt_ws_nl, string("::").map(str2symbol), opt_ws_nl)
    .or(call_op)
    .parse_stream(i)
}

fn rparen<I>(i: I) -> ParseResult<(), I> where I: Stream<Item = char> {
  (opt_ws_nl, token(')')).parse_stream(i)
}
fn rbracket<I>(i: I) -> ParseResult<(), I> where I: Stream<Item = char> {
  (opt_ws_nl, token(']')).parse_stream(i)
}

fn term<I>(i: I) -> ParseResult<(), I> where I: Stream<Item = char> {
  (opt_ws, token(";"), opt_ws)
    .or(nl)
    .or(heredoc_body)
    .parse_stream(i)
}

fn comment<I>(i: I) -> ParseResult<String, I> where I: Stream<Item = char> {
  (opt_ws, token('#'), many(none_of("\n")), token('\n'))
    .map(|_, _, com, _| String::from_utf8(com).unwrap())
    .parse_stream(i)
}

fn nl<I>(i: I) -> ParseResult<(), I> where I: Stream<Item = char> {
  (opt_ws, token('\n'), opt_ws).map(|_, _, _| ())
    .or(comment)
    .parse_stream(i)
}