#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(non_shorthand_field_patterns)]

use structopt::StructOpt;
use anyhow;
use either::{Left, Right};

use std::path::PathBuf;
use std::fs;
use std::fs::File;
use std::rc::Rc;
use std::ops::Deref;
use std::borrow::Cow;
use std::io::BufReader;
use std::io::BufRead;
use std::io::BufWriter;
use std::io::Write;
use std::mem::transmute;


// The actual parser is over here
mod errors;
mod tokenizer;
use tokenizer::tokenize;
use tokenizer::tokenize_at;
use tokenizer::Token;
use tokenizer::Tt;
use tokenizer::tok;
mod pool;
use pool::Pool;
use pool::Swim;
mod parser;
use parser::parse;
use parser::Expr;
use parser::sym;
use parser::span;


// edit the tree
fn edit<'b, 'a>(
    o: &mut Pool<'b>,
    expr: Expr<'b, 'a>
) -> Result<Expr<'b, 'a>, anyhow::Error> {
    // lfsr_data_t
    if let
        expr@Expr::Decl(
            Expr::Sym(Token{tok: "lfs_size_t", ..}),
            Token{tok: "weight_", ..},
        ) = expr
    {
        let data = sym("lfsr_data_t data_").indent(o, expr.col()-1);
        return Ok(span(o, &[
            expr,
            data,
        ]));
    }

    // lfsr_btree_get
    if let
        Expr::Binary(
            Expr::Call(
                Expr::Sym(sym_@Token{tok: "lfsr_btree_get", ..}),
                lp,
                &[
                    lfs,
                    btree,
                    bid,
                    tag,
                    weight,
                    buffer,
                    size
                ],
                rp,
            ),
            arrow@Token{tt: Tt::BigArrow, ..},
            rh
        ) = expr
    {
        // left => no error
        // right => error
        let rh = match rh {
            rh@Expr::Sym(sym_) if sym_.tok.starts_with("LFS_ERR_") => Right(rh),
            rh => Left(rh),
        };

        let mut list_ = vec![];
        list_.push(Expr::Binary(
            Expr::Call(
                sym("lfsr_btree_lookup").lws_(o, sym_.lws).swim(o),
                *lp,
                [
                    lfs,
                    btree,
                    bid,
                    tag,
                    weight,
                    (Some(sym("&data_").lws_(o, " ")), None)
                ].swim(o),
                *rp,
            ).swim(o),
            arrow.lws_(" "),
            match rh {
                Left(_) => sym("0").lws_(o, " ").swim(o),
                Right(rh) => rh,
            },
        ));

        if let Left(rh) = rh {
            list_.push(Expr::Binary(
                Expr::Call(
                    sym("lfsr_data_read").indent(o, sym_.col-1).swim(o),
                    *lp,
                    [
                        (Some(sym("&lfs")), Some(tok(","))),
                        (Some(sym("&data_").lws_(o, " ")), Some(tok(","))),
                        buffer,
                        size,
                    ].swim(o),
                    *rp
                ).swim(o),
                tok("=>").lws_(" "),
                rh.swim(o),
            ));
        }

        return Ok(span(o, &list_));
    }

    // lfsr_btree_push
    fn strip<'b, 'a>(
        o: &mut Pool<'b>,
        expr: Expr<'b, 'a>,
        prefix: &'static str
    ) -> Expr<'b, 'a> {
        match expr {
            Expr::Call(
                Expr::Sym(Token{tok: tok, ..}),
                lp,
                args,
                rp,
            ) if tok.starts_with(prefix) => {
                Expr::Call(
                    sym(tok.strip_prefix(prefix).unwrap()).swim(o),
                    lp,
                    args,
                    rp
                )
            }
            Expr::Sym(Token{tok: tok, ..}) if tok.starts_with(prefix) => {
                sym(tok.strip_prefix(prefix).unwrap())
            }
            expr => expr,
        }
    }

    fn prefix<'b, 'a>(
        o: &mut Pool<'b>,
        expr: Expr<'b, 'a>,
        prefix: &'static str,
        escape: &'static str,
    ) -> Expr<'b, 'a> {
        match expr {
            Expr::Call(
                Expr::Sym(Token{tok: tok, ..}),
                lp,
                args,
                rp,
            ) if tok.starts_with(prefix) => {
                Expr::Call(
                    sym(tok.strip_prefix(prefix).unwrap()).swim(o),
                    lp,
                    args,
                    rp
                )
            }
            Expr::Sym(Token{tok: tok, ..}) if tok.starts_with(prefix) => {
                sym(tok.strip_prefix(prefix).unwrap())
            }
            expr => {
                Expr::Call(
                    sym(escape).swim(o),
                    tok("("),
                    [(Some(expr), None)].swim(o),
                    tok(")")
                )
            }
        }
    }

    if let
        Expr::Call(
            Expr::Sym(sym_@Token{tok: "lfsr_btree_push", ..}),
            lp,
            &[
                lfs,
                btree,
                (Some(bid), _),
                (Some(tag), _),
                (Some(weight), _),
                (Some(data), _)
            ],
            rp,
        ) = expr
    {
        return Ok(Expr::Call(
            sym("lfsr_btree_commit").lws_(o, sym_.lws).col_(o, sym_.col).swim(o),
            lp,
            [
                lfs,
                btree,
                (Some(bid.lws_(o, " ")), Some(tok(","))),
                (Some(Expr::Call(
                    sym("LFSR_ATTRS").lws_(o, " ").swim(o),
                    tok("("),
                    [
                        (Some(Expr::Call(
                            sym("LFSR_ATTR").swim(o),
                            tok("("),
                            [
                                (Some(
                                    prefix(o, tag, "LFSR_TAG_", "TAG").lws_(o, "")
                                ), Some(tok(","))),
                                (Some(Expr::Unary(
                                    tok("+").lws_(" "),
                                    weight.lws_(o, "").swim(o)
                                )), Some(tok(","))),
                                (Some(
                                    prefix(o, data, "LFSR_DATA_", "DATA").lws_(o, " ")
                                ), None)
                            ].swim(o),
                            tok(")")
                        )), None)
                    ].swim(o),
                    tok(")")
                )), None)
            ].swim(o),
            rp,
        ));
    }

    // lfsr_btree_set
    if let
        Expr::Call(
            Expr::Sym(sym_@Token{tok: "lfsr_btree_set", ..}),
            lp,
            &[
                lfs,
                btree,
                (Some(bid), _),
                (Some(tag), _),
                (Some(delta), _),
                (Some(data), _)
            ],
            rp,
        ) = expr
    {
        let mut list_ = vec![];
        list_.push((Some(Expr::Call(
            sym("LFSR_ATTR").swim(o),
            tok("("),
            [
                (Some(Expr::Call(
                    sym("SUBMASK").swim(o),
                    tok("("),
                    [(Some(prefix(o, tag, "LFSR_TAG_", "TAG").lws_(o, "")), None)].swim(o),
                    tok(")")
                )), Some(tok(","))),
                (Some(
                    sym("0").lws_(o, " ")
                ), Some(tok(","))),
                (Some(
                    prefix(o, data, "LFSR_DATA_", "DATA").lws_(o, " ")
                ), None)
            ].swim(o),
            tok(")")
        )), None));

        match delta {
            Expr::Lit(Token{tok: "0", ..}) => {}
            _ => {
                list_.last_mut().unwrap().1 = Some(tok(","));
                list_.push((Some(Expr::Call(
                    sym("LFSR_ATTR").swim(o),
                    tok("("),
                    [
                        (Some(
                            sym("GROW")
                        ), Some(tok(","))),
                        (Some(
                            delta.lws_(o, " ")
                        ), Some(tok(","))),
                        (Some(
                            sym("NULL()").lws_(o, " ")
                        ), None)
                    ].swim(o),
                    tok(")")
                )), None));
            }
        }

        return Ok(Expr::Call(
            sym("lfsr_btree_commit").lws_(o, sym_.lws).col_(o, sym_.col).swim(o),
            lp,
            [
                lfs,
                btree,
                (Some(bid.lws_(o, " ")), Some(tok(","))),
                (Some(Expr::Call(
                    sym("LFSR_ATTRS").lws_(o, " ").swim(o),
                    tok("("),
                    list_.swim(o),
                    tok(")")
                )), None)
            ].swim(o),
            rp,
        ));
    }

    // lfsr_btree_pop
    if let
        Expr::Call(
            Expr::Sym(sym_@Token{tok: "lfsr_btree_pop", ..}),
            lp,
            &[
                lfs,
                btree,
                (Some(bid), _),
                (Some(delta), _),
            ],
            rp,
        ) = expr
    {
        let mut list_ = vec![];
        list_.push((Some(Expr::Call(
            sym("LFSR_ATTR").swim(o),
            tok("("),
            [
                (Some(
                    sym("RM")
                ), Some(tok(","))),
                (Some(
                    delta.lws_(o, " ")
                ), Some(tok(","))),
                (Some(
                    sym("NULL()").lws_(o, " ")
                ), None)
            ].swim(o),
            tok(")")
        )), None));

        return Ok(Expr::Call(
            sym("lfsr_btree_commit").lws_(o, sym_.lws).col_(o, sym_.col).swim(o),
            lp,
            [
                lfs,
                btree,
                (Some(bid.lws_(o, " ")), Some(tok(","))),
                (Some(Expr::Call(
                    sym("LFSR_ATTRS").lws_(o, " ").swim(o),
                    tok("("),
                    list_.swim(o),
                    tok(")")
                )), None)
            ].swim(o),
            rp,
        ));
    }

    // lfsr_btree_split
    if let
        Expr::Call(
            Expr::Sym(sym_@Token{tok: "lfsr_btree_split", ..}),
            lp,
            &[
                lfs,
                btree,
                (Some(bid), _),
                (Some(name), _),
                (Some(tag1), _),
                (Some(delta1), _),
                (Some(data1), _),
                (Some(tag2), _),
                (Some(delta2), _),
                (Some(data2), _),
            ],
            rp,
        ) = expr
    {
        let mut list_ = vec![];
        match delta1 {
            Expr::Lit(Token{tok: "0", ..}) => {}
            _ => {
                list_.push((Some(Expr::Call(
                    sym("LFSR_ATTR").swim(o),
                    tok("("),
                    [
                        (Some(
                            sym("GROW")
                        ), Some(tok(","))),
                        (Some(
                            delta1.lws_(o, " ")
                        ), Some(tok(","))),
                        (Some(
                            sym("NULL()").lws_(o, " ")
                        ), None)
                    ].swim(o),
                    tok(")")
                )), None));
            }
        }

        list_.last_mut().map(|last| last.1 = Some(tok(",")));
        list_.push((Some(Expr::Call(
            sym("LFSR_ATTR").swim(o),
            tok("("),
            [
                (Some(
                    prefix(o, tag1, "LFSR_TAG_", "TAG").lws_(o, "")
                ), Some(tok(","))),
                (Some(
                    sym("0").lws_(o, " ")
                ), Some(tok(","))),
                (Some(
                    prefix(o, data1, "LFSR_DATA_", "DATA").lws_(o, " ")
                ), None)
            ].swim(o),
            tok(")")
        )), None));

        match name {
            Expr::Call(
                Expr::Sym(Token{tok: "LFSR_DATA_NULL", ..}),
                ..
            ) => {
                list_.last_mut().map(|last| last.1 = Some(tok(",")));
                list_.push((Some(Expr::Call(
                    sym("LFSR_ATTR").swim(o),
                    tok("("),
                    [
                        (Some(
                            prefix(o, tag2, "LFSR_TAG_", "TAG").lws_(o, "")
                        ), Some(tok(","))),
                        (Some(
                            delta2.lws_(o, " ")
                        ), Some(tok(","))),
                        (Some(
                            prefix(o, data2, "LFSR_DATA_", "DATA").lws_(o, " ")
                        ), None)
                    ].swim(o),
                    tok(")")
                )), None));
            }
            _ => {
                list_.last_mut().map(|last| last.1 = Some(tok(",")));
                list_.push((Some(Expr::Call(
                    sym("LFSR_ATTR").swim(o),
                    tok("("),
                    [
                        (Some(
                            sym("NAME")
                        ), Some(tok(","))),
                        (Some(
                            delta2.lws_(o, " ")
                        ), Some(tok(","))),
                        (Some(
                            prefix(o, name, "LFSR_DATA_", "DATA").lws_(o, " ")
                        ), None)
                    ].swim(o),
                    tok(")")
                )), Some(tok(","))));
                list_.push((Some(Expr::Call(
                    sym("LFSR_ATTR").swim(o),
                    tok("("),
                    [
                        (Some(
                            prefix(o, tag2, "LFSR_TAG_", "TAG").lws_(o, "")
                        ), Some(tok(","))),
                        (Some(
                            sym("0").lws_(o, " ")
                        ), Some(tok(","))),
                        (Some(
                            prefix(o, data2, "LFSR_DATA_", "DATA").lws_(o, " ")
                        ), None)
                    ].swim(o),
                    tok(")")
                )), None));
            }
        }

        return Ok(Expr::Call(
            sym("lfsr_btree_commit").lws_(o, sym_.lws).col_(o, sym_.col).swim(o),
            lp,
            [
                lfs,
                btree,
                (Some(bid.lws_(o, " ")), Some(tok(","))),
                (Some(Expr::Call(
                    sym("LFSR_ATTRS").lws_(o, " ").swim(o),
                    tok("("),
                    list_.swim(o),
                    tok(")")
                )), None)
            ].swim(o),
            rp,
        ));
    }

    // fix indention issues
    if let
        Expr::Binary(
            Expr::Call(
                sym_@Expr::Sym(Token{tok: "lfsr_btree_commit", ..}),
                lp,
                &[
                    lfs,
                    btree,
                    bid,
                    (Some(Expr::Call(
                        sym__@Expr::Sym(Token{tok: "LFSR_ATTRS", ..}),
                        lp_,
                        attrs@&[
                            (Some(first), _),
                            ..
                        ],
                        rp_
                    )), comma)
                ],
                rp,
            ),
            arrow@Token{tt: Tt::BigArrow, ..},
            rh
        ) = expr
    {
        if first.lws() == "" {
            return Ok(Expr::Binary(
                Expr::Call(
                    sym_,
                    *lp,
                    [
                        lfs,
                        btree,
                        bid,
                        (Some(Expr::Call(
                            sym__,
                            lp_,
                            {
                                let mut list_ = vec![];
                                for attr in attrs {
                                    match attr {
                                        (Some(attr), comma_) => list_.push((Some(attr.indent(o, sym_.col()-1+8)), *comma_)),
                                        attr => list_.push(*attr),
                                    }
                                }
                                list_
                            }.swim(o),
                            rp_
                        )), comma)
                    ].swim(o),
                    *rp,
                ).swim(o),
                arrow,
                rh
            ));
        }
    }

    if let
        Expr::Binary(
            lh,
            eq@Token{tt: Tt::Assign, ..},
            Expr::Call(
                sym_@Expr::Sym(Token{tok: "lfsr_btree_commit", ..}),
                lp,
                &[
                    lfs,
                    btree,
                    bid,
                    (Some(Expr::Call(
                        sym__@Expr::Sym(Token{tok: "LFSR_ATTRS", ..}),
                        lp_,
                        attrs@&[
                            (Some(first), _),
                            ..
                        ],
                        rp_
                    )), comma)
                ],
                rp,
            )
        ) = expr
    {
        if first.lws() == "" {
            return Ok(Expr::Binary(
                lh,
                eq,
                Expr::Call(
                    sym_,
                    *lp,
                    [
                        lfs,
                        btree,
                        bid,
                        (Some(Expr::Call(
                            sym__,
                            lp_,
                            {
                                let mut list_ = vec![];
                                for attr in attrs {
                                    match attr {
                                        (Some(attr), comma_) => list_.push((Some(attr.indent(o, lh.col()-1+8)), *comma_)),
                                        attr => list_.push(*attr),
                                    }
                                }
                                list_
                            }.swim(o),
                            rp_
                        )), comma)
                    ].swim(o),
                    *rp,
                ).swim(o)
            ));
        }
    }
        

    Ok(expr)
}




// CLI arguments
fn parse_rc_path(s: &std::ffi::OsStr) -> Rc<PathBuf> {
    Rc::new(PathBuf::from(s))
}

#[derive(Debug, StructOpt)]
#[structopt(rename_all="kebab")]
struct Opt {
    #[structopt(parse(from_os_str=parse_rc_path))]
    input: Rc<PathBuf>,

    #[structopt(short, long)]
    output: Option<PathBuf>,

    #[structopt(long)]
    dump_tokens: bool,

    #[structopt(long)]
    dump_tree: bool,

    #[structopt(long)]
    dump_modified: bool,

    #[structopt(short="t", long)]
    in_toml: bool,
}

// entry point
fn main() -> Result<(), anyhow::Error> {
    let opt = Opt::from_args();

    if opt.in_toml {
        let f = File::open(opt.input.as_ref())?;
        let f = BufReader::new(f);
        if let Some(output) = opt.output {
            let mut f_ = File::create(output)?;
            let mut line = 1;
            let mut c_line = 1;
            let mut in_c = false;
            let mut chunk = String::new();
            for line_ in f.lines() {
                let line_ = line_?;
                // switch modes
                if in_c && line_.starts_with("'''") {
                    // tokenize
                    let tokens = match tokenize_at(
                        &opt.input, c_line, 1, &chunk
                    ) {
                        Ok(tokens) => tokens,
                        Err(err) => {
                            err.print_context();
                            std::process::exit(1);
                        }
                    };

                    if opt.dump_tokens {
                        println!("{:#?}", tokens);
                    }

                    // parse
                    let mut tree = match parse(&opt.input, &tokens) {
                        Ok(tree) => tree,
                        Err(err) => {
                            err.print_context();
                            std::process::exit(1);
                        }
                    };

                    if opt.dump_tree {
                        println!("{:#?}", tree);
                    }

                    // edit!
                    tree = tree.try_map_exprs(edit)?;

                    if opt.dump_modified {
                        println!("{:#?}", tree);
                    }

                    // flatten and write to file
                    tree.try_visit_tokens(|tok| {
                        // make sure to keep whitespace!
                        write!(f_, "{}", tok.lws)?;
                        write!(f_, "{}", tok.tok)?;
                        Ok::<_, anyhow::Error>(())
                    })?;

                    drop(tree);
                    in_c = false;
                    chunk.clear();
                    writeln!(f_, "{}", line_)?;
                } else if in_c {
                    chunk.push_str(&line_);
                    chunk.push('\n');

                } else {
                    writeln!(f_, "{}", line_)?;

                    // switch modes
                    if line_.starts_with("code = '''") {
                        in_c = true;
                        c_line = line+1;
                        chunk.clear();
                    }
                }
                line += 1;
            }
        }

    } else {
        let input = fs::read_to_string(opt.input.as_ref())?;

        // tokenize
        let tokens = match tokenize(&opt.input, &input) {
            Ok(tokens) => tokens,
            Err(err) => {
                err.print_context();
                std::process::exit(1);
            }
        };

        if opt.dump_tokens {
            println!("{:#?}", tokens);
        }

        // parse
        let tree = match parse(&opt.input, &tokens) {
            Ok(tree) => tree,
            Err(err) => {
                err.print_context();
                std::process::exit(1);
            }
        };

        if opt.dump_tree {
            println!("{:#?}", tree);
        }

        // edit!
        let tree = tree.try_map_exprs(edit)?;

        if opt.dump_modified {
            println!("{:#?}", tree);
        }

        // flatten and write to file
        if let Some(output) = opt.output {
            let f = File::create(output)?;
            let mut f = BufWriter::new(f);
            tree.try_visit_tokens(|tok| {
                // make sure to keep whitespace!
                write!(f, "{}", tok.lws)?;
                write!(f, "{}", tok.tok)?;
                Ok::<_, anyhow::Error>(())
            })?;
        }
    }

    Ok(())
}
