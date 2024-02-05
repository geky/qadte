
use either::{Left, Right};

use std::ops::Deref;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use crate::rc::Rc;
use crate::rc::Transborrow;
use crate::tokenizer::Token;
use crate::tokenizer::Tt;
use crate::tokenizer::tok;
use crate::parser::{Expr, Expr_};
use crate::parser::sym;
use crate::parser::span;
use crate::parser::Map;

// edit the tree
pub fn edit<'a>(expr: Expr<'a>) -> Result<Expr<'a>, anyhow::Error> {
    static COUNTER: AtomicUsize = AtomicUsize::new(1);

    // <expr> = <commit>(<args>, LFS_MKATTRS(*), <args>);
    if let
        Expr_::Binary(
            ret,
            eq@Token{tt: Tt::Assign, ..},
            Expr_::Call(
                commit,
                lp,
                args,
                rp,
            )
        ) = expr.borrow()
    {
        for i in 0..args.len() {
            if let
                (Some(Expr_::Call(
                    Expr_::Sym(Token{tok: "LFS_MKATTRS", ..}),
                    lp_,
                    attrs,
                    rp_,
                )), c@_) = args[i]
            {
                let count = attrs.len();
                let unique = COUNTER.fetch_add(1, Ordering::AcqRel);

                return Ok(span(&[
                    Expr::Binary(
                        Rc::new(sym(format!("struct lfs_mattr attrs{}[{}]", unique, count))),
                        tok("=").lws_(" "),
                        Rc::new(Expr::Squiggle(
                            tok("{").lws_(" "),
                            Rc::from(attrs.map(|tok: Token<'_>| {
                                if tok.lws.starts_with("\n") {
                                    tok.indent(tok.col-1-4)
                                } else {
                                    tok
                                }
                            })),
                            tok("}").indent(ret.col()-1),
                        )),
                    ).lws_(ret.lws()),

                    Expr::Binary(
                        Rc::new(ret.deref().into()),
                        *eq,
                        Rc::new(Expr::Call(
                            Rc::new(commit.deref().into()),
                            *lp,
                            Rc::from(
                                args[..i].iter()
                                    .map(|(arg, c)| (arg.map(|arg| Expr::from(arg)), *c))
                                    .chain([
                                        (Some(sym(format!("attrs{}", unique)).lws_(" ")), Some(tok(","))),
                                        (Some(sym(format!("{}", count)).lws_(" ")), c)
                                    ])
                                    .chain(
                                        args[i+1..].iter()
                                            .map(|(arg, c)| (arg.map(|arg| Expr::from(arg)), *c))
                                    )
                                    .collect::<Vec<_>>()
                            ),
                            *rp,
                        ))
                    ).indent(ret.col()-1)
                ]));
            }
        }
    }

    // return <commit>(<args>, LFS_MKATTRS(*), <args>);
    if let
        ret@Expr_::Return(
            ret_,
            Some(Expr_::Call(
                commit,
                lp,
                args,
                rp,
            ))
        ) = expr.borrow()
    {
        for i in 0..args.len() {
            if let
                (Some(Expr_::Call(
                    Expr_::Sym(Token{tok: "LFS_MKATTRS", ..}),
                    lp_,
                    attrs,
                    rp_,
                )), c@_) = args[i]
            {
                let count = attrs.len();

                return Ok(span(&[
                    Expr::Binary(
                        Rc::new(sym(format!("struct lfs_mattr attrs_[{}]", count))),
                        tok("=").lws_(" "),
                        Rc::new(Expr::Squiggle(
                            tok("{").lws_(" "),
                            Rc::from(attrs.map(|tok: Token<'_>| {
                                if tok.lws.starts_with("\n") {
                                    tok.indent(tok.col-1-4)
                                } else {
                                    tok
                                }
                            })),
                            tok("}").indent(ret.col()-1),
                        )),
                    ).lws_(ret.lws()),

                    Expr::Return(
                        *ret_,
                        Some(Rc::new(Expr::Call(
                            Rc::new(commit.deref().into()),
                            *lp,
                            Rc::from(
                                args[..i].iter()
                                    .map(|(arg, c)| (arg.map(|arg| Expr::from(arg)), *c))
                                    .chain([
                                        (Some(sym("attrs_").lws_(" ")), Some(tok(","))),
                                        (Some(sym(format!("{}", count)).lws_(" ")), c)
                                    ])
                                    .chain(
                                        args[i+1..].iter()
                                            .map(|(arg, c)| (arg.map(|arg| Expr::from(arg)), *c))
                                    )
                                    .collect::<Vec<_>>()
                            ),
                            *rp,
                        )))
                    ).indent(ret.col()-1)
                ]));
            }
        }
    }

    Ok(expr)
}

