use aleph_syntax_tree::syntax::AlephTree as at;

fn gen(ast: at, indent: i64) -> String {
    let c_indent = aleph_syntax_tree::comp_indent(indent);
    match ast {
        at::Unit => String::new(),
        at::Break => format!("{}break", c_indent),
        at::Continue => format!("{}continue", c_indent),
        at::Ellipsis => "...".to_string(),
        at::Int{value} => format!("{}{}", c_indent, value),
        at::Float{value} => format!("{}{}", c_indent, value),
        at::Bool{value} => format!("{}{}", c_indent, if value == "true" { "true" } else { "false" }),
        at::String{value} => format!("{}{}", c_indent, value),
        at::Ident{value} => format!("{}{}", c_indent, value),
        at::Complex{real, imag} => format!("{}{{complex, {}, {}}}", c_indent, real, imag),
        at::Bytes{elems} => format!("<<{}>>", elems.iter().map(|b| b.to_string()).collect::<Vec<_>>().join(",")),
        at::Tuple{elems} => format!("{{{}}}", aleph_syntax_tree::gen_list_expr_sep(elems, gen, ", ")),
        at::Array{elems} => format!("[{}]", aleph_syntax_tree::gen_list_expr_sep(elems, gen, ", ")),
        at::Neg{expr} => format!("{}-{}", c_indent, gen(*expr, 0)),
        at::Not{bool_expr} => format!("{}not {}", c_indent, gen(*bool_expr, 0)),
        at::And{bool_expr1, bool_expr2} => format!("{}{} andalso {}", c_indent, gen(*bool_expr1, 0), gen(*bool_expr2, 0)),
        at::Or{bool_expr1, bool_expr2} => format!("{}{} orelse {}", c_indent, gen(*bool_expr1, 0), gen(*bool_expr2, 0)),
        at::Add{number_expr1, number_expr2} => format!("{}{} + {}", c_indent, gen(*number_expr1, 0), gen(*number_expr2, 0)),
        at::Sub{number_expr1, number_expr2} => format!("{}{} - {}", c_indent, gen(*number_expr1, 0), gen(*number_expr2, 0)),
        at::Mul{number_expr1, number_expr2} => format!("{}{} * {}", c_indent, gen(*number_expr1, 0), gen(*number_expr2, 0)),
        at::Div{number_expr1, number_expr2} => format!("{}{} / {}", c_indent, gen(*number_expr1, 0), gen(*number_expr2, 0)),
        at::Eq{expr1, expr2} => format!("{}{} =:= {}", c_indent, gen(*expr1, 0), gen(*expr2, 0)),
        at::LE{expr1, expr2} => format!("{}{} =< {}", c_indent, gen(*expr1, 0), gen(*expr2, 0)),
        at::In{expr1, expr2} => format!("{}lists:member({}, {})", c_indent, gen(*expr1, 0), gen(*expr2, 0)),
        at::If{condition, then, els} => match *els {
            at::Unit => format!("{}case {} of\n{}true -> {}\nend",
                c_indent, gen(*condition, 0), aleph_syntax_tree::comp_indent(indent+1), gen(*then, 0)),
            _ => format!("{}case {} of\n{}true -> {};\n{}false -> {}\nend",
                c_indent, gen(*condition, 0),
                aleph_syntax_tree::comp_indent(indent+1), gen(*then, 0),
                aleph_syntax_tree::comp_indent(indent+1), gen(*els, 0)),
        },
        at::While{init_expr, condition, loop_expr, post_expr} => {
            let init = match *init_expr { at::Unit => String::new(), e => format!("{},\n", gen(e, indent)) };
            let post = match *post_expr { at::Unit => String::new(), e => format!(",\n{}", gen(e, indent)) };
            format!("{}{}while_loop(fun() -> {} end, fun() -> {} end){}",
                c_indent, init, gen(*condition, 0), gen(*loop_expr, 0), post)
        },
        at::Let{var, value, expr, ..} => match *expr {
            at::Unit => format!("{}{} = {}", c_indent, var, gen(*value, 0)),
            e => format!("{}{} = {},\n{}", c_indent, var, gen(*value, 0), gen(e, indent)),
        },
        at::LetRec{name, args, body} => format!("{}{}({}) ->\n{}.",
            c_indent, name, aleph_syntax_tree::gen_list_expr(args, gen), gen(*body, indent+1)),
        at::Get{array_name, elem} => format!("{}lists:nth({} + 1, {})", c_indent, gen(*elem, 0), array_name),
        at::Put{array_name, elem, value, insert} => {
            if insert == "true" {
                format!("{}lists:insert({}, {}, {})", c_indent, gen(*elem, 0), gen(*value, 0), array_name)
            } else {
                let e = gen(*elem, 0);
                format!("{}lists:sublist({}, {}) ++ [{}] ++ lists:nthtail({} + 1, {})",
                    c_indent, array_name, e, gen(*value, 0), e, array_name)
            }
        },
        at::Remove{array_name, elem, is_value} => {
            if is_value == "true" {
                format!("{}lists:delete({}, {})", c_indent, gen(*elem, 0), array_name)
            } else {
                let e = gen(*elem, 0);
                format!("{}lists:sublist({}, {} - 1) ++ lists:nthtail({}, {})",
                    c_indent, array_name, e, e, array_name)
            }
        },
        at::Length{var} => format!("{}length({})", c_indent, var),
        at::Match{expr, case_list} => format!("{}case {} of\n{}\nend",
            c_indent, gen(*expr, 0),
            case_list.iter().map(|c| gen(*c.clone(), indent+1)).collect::<Vec<_>>().join(";\n")),
        at::MatchLine{condition, case_expr} => format!("{}{} -> {}", c_indent, gen(*condition, 0), gen(*case_expr, 0)),
        at::Var{var, ..} => format!("{}{}", c_indent, var),
        at::App{object_name, fun, param_list} => {
            let prefix = if !object_name.is_empty() { format!("{}:", object_name) } else { String::new() };
            format!("{}{}{}({})", c_indent, prefix, gen(*fun, 0), aleph_syntax_tree::gen_list_expr(param_list, gen))
        },
        at::Stmts{expr1, expr2} => format!("{},\n{}", gen(*expr1, indent), gen(*expr2, indent)),
        at::Iprt{name, ..} => format!("{}-include(\"{}.hrl\").", c_indent, name),
        at::Clss{name, attribute_list, body, ..} => format!(
            "{}%% Module: {}\n{}-module({}).\n{}-export([{}]).\n\n{}",
            c_indent, name, c_indent, name, c_indent, attribute_list.join(", "), gen(*body, indent)),
        at::Return{value} => gen(*value, indent),
        at::Comment{value} => format!("{}{}", c_indent, value),
        at::CommentMulti{value} => format!("{}{}", c_indent, value),
        at::Assert{condition, message} => format!(
            "{}case {} of\n{}true -> ok;\n{}false -> error({})\nend",
            c_indent, gen(*condition, 0),
            aleph_syntax_tree::comp_indent(indent+1),
            aleph_syntax_tree::comp_indent(indent+1), gen(*message, 0)),
        at::Move{source, targets, ..} => format!("{}%% MOVE {} TO {}",
            c_indent, gen(*source, 0),
            targets.iter().map(|t| gen(*t.clone(), 0)).collect::<Vec<_>>().join(", ")),
        at::Compute{target, expression, on_error, ..} => {
            let err = match on_error { Some(e) => format!(" %% on_error {}", gen(*e, 0)), None => String::new() };
            format!("{}{} = {}{}", c_indent, gen(*target, 0), gen(*expression, 0), err)
        },
        at::GoTo{target, depending_on, ..} => {
            let dep = match depending_on { Some(d) => format!(" depending on {}", gen(*d, 0)), None => String::new() };
            format!("{}%% GO TO {}{}", c_indent, target, dep)
        },
        at::Exit => format!("{}exit(normal)", c_indent),
        at::Section{name, content, ..} => format!("{}%% Section: {}\n{}",
            c_indent, name,
            content.iter().map(|p| gen(*p.clone(), indent)).collect::<Vec<_>>().join("\n\n")),
        at::Perform{target, times, inline, ..} => {
            let t = target.unwrap_or_else(|| "inline".to_string());
            let ti = match times { Some(e) => format!(" times {}", gen(*e, 0)), None => String::new() };
            let body = match inline {
                Some(stmts) => format!("\n{}", stmts.iter().map(|s| gen(*s.clone(), indent+1)).collect::<Vec<_>>().join(",\n")),
                None => String::new(),
            };
            format!("{}perform({}{}){}", c_indent, t, ti, body)
        },
        at::Accept{entry_name, ..} => format!("{}{} = io:get_line(\"\")", c_indent, entry_name),
        at::Figurative{figurative_type} => match figurative_type.as_str() {
            "SPACES" => format!("{}\" \"", c_indent),
            "ZEROS" | "ZEROES" => format!("{}0", c_indent),
            "HIGH-VALUES" => format!("{}<<255>>", c_indent),
            "LOW-VALUES" => format!("{}<<0>>", c_indent),
            _ => format!("{}{}", c_indent, figurative_type),
        },
        at::HexLiteral{value} => format!("{}<<16#{}>", c_indent, value),
        _ => String::new(),
    }
}

pub fn generate(ast: at) -> String {
    gen(ast, 0)
}
