use aleph_syntax_tree::syntax::AlephTree as at;

fn gen(ast: at, indent: i64) -> String {
    let c_indent = aleph_syntax_tree::comp_indent(indent);
    match ast {
        at::Unit => format!(""),
        at::Break => format!("{}break", c_indent),
        at::Continue => format!("{}continue", c_indent),
        at::Ellipsis => format!("..."),
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
            at::Unit => format!("{}case {} of\n{}true -> {}\nend", c_indent, gen(*condition, 0), aleph_syntax_tree::comp_indent(indent+1), gen(*then, 0)),
            _ => format!("{}case {} of\n{}true -> {};\n{}false -> {}\nend", c_indent, gen(*condition, 0), aleph_syntax_tree::comp_indent(indent+1), gen(*then, 0), aleph_syntax_tree::comp_indent(indent+1), gen(*els, 0)),
        },
        at::While{init_expr, condition, loop_expr, post_expr} => {
            let init = match *init_expr { at::Unit => String::new(), _ => format!("{},\n", gen(*init_expr, indent)) };
            let post = match *post_expr { at::Unit => String::new(), _ => format!(",\n{}", gen(*post_expr, indent)) };
            format!("{}{}while_loop(fun() -> {} end, fun() -> {} end){}",
                c_indent, init, gen(*condition, 0), gen(*loop_expr, 0), post)
        },
        at::Let{var, is_pointer: _, value, expr} => match *expr {
            at::Unit => format!("{}{} = {}", c_indent, var, gen(*value, 0)),
            _ => format!("{}{} = {},\n{}", c_indent, var, gen(*value, 0), gen(*expr, indent))
        },
        at::LetRec{name, args, body} => format!("{}{}({}) ->\n{}.", c_indent, name, aleph_syntax_tree::gen_list_expr(args, gen), gen(*body, indent+1)),
        at::Get{array_name, elem} => format!("{}lists:nth({} + 1, {})", c_indent, gen(*elem, 0), array_name),
        at::Put { array_name, elem, value, insert } => {
            if insert == "true" {
                format!("{}lists:insert({}, {}, {})", c_indent, gen(*elem, 0), gen(*value.clone(), 0), array_name)
            } else {
                let elem_str = gen(*elem, 0);
                format!("{}lists:sublist({}, {}) ++ [{}] ++ lists:nthtail({} + 1, {})", c_indent, array_name, elem_str, gen(*value.clone(), 0), elem_str, array_name)
            }
        },
        at::Remove { array_name, elem, is_value } => {
            if is_value == "true" {
                format!("{}lists:delete({}, {})", c_indent, gen(*elem, 0), array_name)
            } else {
                let elem_str = gen(*elem, 0);
                format!("{}lists:sublist({}, {} - 1) ++ lists:nthtail({}, {})", c_indent, array_name, elem_str, elem_str, array_name)
            }
        },
        at::Length{var} => format!("{}length({})", c_indent, var),
        at::Match{expr, case_list} => format!("{}case {} of\n{}\nend", c_indent, gen(*expr, 0), 
            case_list.iter().map(|case| gen(*case.clone(), indent+1)).collect::<Vec<_>>().join(";\n")),
        at::MatchLine{condition, case_expr} => format!("{}{} -> {}", c_indent, gen(*condition, 0), gen(*case_expr, 0)),
        at::Var{var, is_pointer: _} => format!("{}{}", c_indent, var),
        at::App{object_name, fun, param_list} => {
            let module_prefix = if !object_name.is_empty() { format!("{}:", object_name) } else { String::new() };
            format!("{}{}{}({})", c_indent, module_prefix, gen(*fun, 0), aleph_syntax_tree::gen_list_expr(param_list, gen))
        },
        at::Stmts{expr1, expr2} => format!("{},\n{}", gen(*expr1, indent), gen(*expr2, indent)),
        at::Iprt{name} => format!("{}-include(\"{}.hrl\").", c_indent, name),
        at::Clss{name, attribute_list, body} => format!("{}%% Module: {}\n{}-module({}).\n{}-export([{}]).\n\n{}", 
            c_indent, name, c_indent, name, c_indent, attribute_list.join(", "), gen(*body, indent)),
        at::Return{value} => gen(*value, indent),
        at::Comment{value} => format!("{}{}", c_indent, value),
        at::CommentMulti{value} => format!("{}{}", c_indent, value),
        at::Assert{condition, message} => format!("{}case {} of\n{}true -> ok;\n{}false -> error({})\nend", 
            c_indent, gen(*condition, 0), aleph_syntax_tree::comp_indent(indent+1), aleph_syntax_tree::comp_indent(indent+1), gen(*message, 0)),

        // COBOL-specific nodes
        at::CobolProgram{program_id, environment_div, data_div, procedure_div} => {
            let env = match environment_div { Some(e) => format!("\n{}", gen(*e, indent)), None => String::new() };
            let data = match data_div { Some(d) => format!("\n{}", gen(*d, indent)), None => String::new() };
            format!("{}%% COBOL Program: {}{}{}\n{}", c_indent, program_id, env, data, gen(*procedure_div, indent))
        },
        at::EnvironmentDivision{config_section, io_control_section} => {
            let config = match config_section { Some(c) => format!("\n{}", gen(*c, indent+1)), None => String::new() };
            let io = match io_control_section { Some(i) => format!("\n{}", gen(*i, indent+1)), None => String::new() };
            format!("{}%% Environment Division{}{}", c_indent, config, io)
        },
        at::DataDivision{file_section, working_storage_section, linkage_section} => {
            let file = match file_section { Some(f) => format!("\n{}", gen(*f, indent+1)), None => String::new() };
            let ws = match working_storage_section { Some(w) => format!("\n{}", gen(*w, indent+1)), None => String::new() };
            let link = match linkage_section { Some(l) => format!("\n{}", gen(*l, indent+1)), None => String::new() };
            format!("{}%% Data Division{}{}{}", c_indent, file, ws, link)
        },
        at::ProcedureDivision{using_clause, statements} => {
            let using = match using_clause { Some(u) => format!(" using {}", u.join(", ")), None => String::new() };
            format!("{}main({}) ->\n{}", c_indent, using, 
                statements.iter().map(|s| gen(*s.clone(), indent+1)).collect::<Vec<_>>().join(",\n"))
        },
        at::PicClause{data_name, level_number, picture, initial_value, occurs_clause, usage} => {
            let init = match initial_value { Some(i) => format!(" = {}", gen(*i, 0)), None => String::new() };
            let occurs = match occurs_clause { Some(o) => format!(" %% occurs {}", o), None => String::new() };
            let use_clause = match usage { Some(u) => format!(" %% usage {}", u), None => String::new() };
            format!("{}%% {} {} pic {}{}{}{}", c_indent, level_number, data_name, picture, init, occurs, use_clause)
        },
        at::GroupItem{data_name, level_number, sub_items} => {
            format!("{}%% {} {} group\n{}", c_indent, level_number, data_name,
                sub_items.iter().map(|s| gen(*s.clone(), indent+1)).collect::<Vec<_>>().join("\n"))
        },
        at::Redefines{data_name, level_number, redefined_item, picture} => {
            let pic = match picture { Some(p) => format!(" pic {}", p), None => String::new() };
            format!("{}%% {} {} redefines {}{}", c_indent, level_number, data_name, redefined_item, pic)
        },
        at::FileDescription{file_name, record_description, block_contains, record_contains} => {
            let block = match block_contains { Some(b) => format!(" block {}", b), None => String::new() };
            let record = match record_contains { Some(r) => format!(" record {}", r), None => String::new() };
            format!("{}%% FD {}{}{}\n{}", c_indent, file_name, block, record,
                record_description.iter().map(|r| gen(*r.clone(), indent+1)).collect::<Vec<_>>().join("\n"))
        },
        at::SelectStatement{file_name, assign_to, access_mode, organization_mode} => {
            let access = match access_mode { Some(a) => format!(" access {}", a), None => String::new() };
            let org = match organization_mode { Some(o) => format!(" organization {}", o), None => String::new() };
            format!("{}%% SELECT {} ASSIGN TO {}{}{}", c_indent, file_name, assign_to, access, org)
        },
        at::Move{source, target_list} => {
            format!("{}%% MOVE {} TO {}", c_indent, gen(*source, 0), 
                target_list.iter().map(|t| gen(*t.clone(), 0)).collect::<Vec<_>>().join(", "))
        },
        at::Compute{target, expression, on_size_error} => {
            let error = match on_size_error { Some(e) => format!(" on_size_error {}", gen(*e, 0)), None => String::new() };
            format!("{}{} = {}{}", c_indent, gen(*target, 0), gen(*expression, 0), error)
        },
        at::Perform{target_paragraph, from_paragraph, through_paragraph, times_clause, until_clause, varying_clause, inline_statements} => {
            let target = match target_paragraph { Some(t) => t.clone(), None => "inline".to_string() };
            let from = match from_paragraph { Some(f) => format!(" from {}", f), None => String::new() };
            let through = match through_paragraph { Some(t) => format!(" through {}", t), None => String::new() };
            let times = match times_clause { Some(t) => format!(" times {}", gen(*t, 0)), None => String::new() };
            let until = match until_clause { Some(u) => format!(" until {}", gen(*u, 0)), None => String::new() };
            let varying = match varying_clause { Some(v) => format!(" varying {}", gen(*v, 0)), None => String::new() };
            let inline = match inline_statements { 
                Some(stmts) => format!("\n{}", stmts.iter().map(|s| gen(*s.clone(), indent+1)).collect::<Vec<_>>().join(",\n")),
                None => String::new()
            };
            format!("{}perform({}{}{}{}{}{}){}", c_indent, target, from, through, times, until, varying, inline)
        },
        at::Accept{target, from_device} => {
            let device = match from_device { Some(d) => format!(" from {}", d), None => String::new() };
            format!("{}{} = io:get_line(\"\"{})", c_indent, gen(*target, 0), device)
        },
        at::Display{item_list, upon_device} => {
            let device = match upon_device { Some(d) => format!(" upon {}", d), None => String::new() };
            format!("{}io:format(\"~p~n\", [{}]){}", c_indent, 
                item_list.iter().map(|i| gen(*i.clone(), 0)).collect::<Vec<_>>().join(", "), device)
        },
        at::Open{mode, file_list} => {
            format!("{}%% OPEN {} {}", c_indent, mode, file_list.join(", "))
        },
        at::Close{file_list} => {
            format!("{}%% CLOSE {}", c_indent, file_list.join(", "))
        },
        at::Read{file_name, into_clause, key_clause, at_end_clause, not_at_end_clause} => {
            let into = match into_clause { Some(i) => format!(" into {}", gen(*i, 0)), None => String::new() };
            let key = match key_clause { Some(k) => format!(" key {}", gen(*k, 0)), None => String::new() };
            let at_end = match at_end_clause { Some(a) => format!(" at_end {}", gen(*a, 0)), None => String::new() };
            let not_at_end = match not_at_end_clause { Some(n) => format!(" not_at_end {}", gen(*n, 0)), None => String::new() };
            format!("{}%% READ {}{}{}{}{}", c_indent, file_name, into, key, at_end, not_at_end)
        },
        at::Write{record_name, from_clause, advancing_clause} => {
            let from = match from_clause { Some(f) => format!(" from {}", gen(*f, 0)), None => String::new() };
            let advancing = match advancing_clause { Some(a) => format!(" advancing {}", a), None => String::new() };
            format!("{}%% WRITE {}{}{}", c_indent, record_name, from, advancing)
        },
        at::GoTo{target_paragraph, depending_on} => {
            let depending = match depending_on { Some(d) => format!(" depending on {}", gen(*d, 0)), None => String::new() };
            format!("{}%% GO TO {}{}", c_indent, target_paragraph, depending)
        },
        at::Stop{stop_type} => {
            format!("{}halt({})", c_indent, stop_type)
        },
        at::Exit => {
            format!("{}exit(normal)", c_indent)
        },
        at::Paragraph{name, statements} => {
            format!("{}{}() ->\n{}", c_indent, name,
                statements.iter().map(|s| gen(*s.clone(), indent+1)).collect::<Vec<_>>().join(",\n"))
        },
        at::Section{name, paragraphs} => {
            format!("{}%% Section: {}\n{}", c_indent, name,
                paragraphs.iter().map(|p| gen(*p.clone(), indent)).collect::<Vec<_>>().join("\n\n"))
        },
        at::Evaluate{selection_subject, when_clauses, when_other} => {
            let other = match when_other { Some(o) => format!(";\n{}{} -> {}", aleph_syntax_tree::comp_indent(indent+1), "_", gen(*o, 0)), None => String::new() };
            format!("{}case {} of\n{}{}\nend", c_indent, gen(*selection_subject, 0),
                when_clauses.iter().map(|w| gen(*w.clone(), indent+1)).collect::<Vec<_>>().join(";\n"), other)
        },
        at::WhenClause{selection_object, statements} => {
            format!("{}{} ->\n{}", c_indent, gen(*selection_object, 0),
                statements.iter().map(|s| gen(*s.clone(), indent+1)).collect::<Vec<_>>().join(",\n"))
        },
        at::Inspect{inspecting_item, tallying_clause, replacing_clause} => {
            let tally = match tallying_clause { Some(t) => format!(" tallying {}", gen(*t, 0)), None => String::new() };
            let replace = match replacing_clause { Some(r) => format!(" replacing {}", gen(*r, 0)), None => String::new() };
            format!("{}%% INSPECT {}{}{}", c_indent, gen(*inspecting_item, 0), tally, replace)
        },
        at::StringStmt{source_items, delimited_by, into_item, with_pointer, on_overflow} => {
            let pointer = match with_pointer { Some(p) => format!(" with pointer {}", gen(*p, 0)), None => String::new() };
            let overflow = match on_overflow { Some(o) => format!(" on overflow {}", gen(*o, 0)), None => String::new() };
            format!("{}%% STRING {} DELIMITED BY {} INTO {}{}{}", c_indent,
                source_items.iter().map(|s| gen(*s.clone(), 0)).collect::<Vec<_>>().join(", "),
                gen(*delimited_by, 0), gen(*into_item, 0), pointer, overflow)
        },
        at::Unstring{source_item, delimited_by, into_items, with_pointer, on_overflow} => {
            let pointer = match with_pointer { Some(p) => format!(" with pointer {}", gen(*p, 0)), None => String::new() };
            let overflow = match on_overflow { Some(o) => format!(" on overflow {}", gen(*o, 0)), None => String::new() };
            format!("{}%% UNSTRING {} DELIMITED BY {} INTO {}{}{}", c_indent,
                gen(*source_item, 0), gen(*delimited_by, 0),
                into_items.iter().map(|i| gen(*i.clone(), 0)).collect::<Vec<_>>().join(", "), pointer, overflow)
        },
        at::OnSizeError{statements} => {
            format!("{}%% ON SIZE ERROR\n{}", c_indent,
                statements.iter().map(|s| gen(*s.clone(), indent+1)).collect::<Vec<_>>().join(",\n"))
        },
        at::NotOnSizeError{statements} => {
            format!("{}%% NOT ON SIZE ERROR\n{}", c_indent,
                statements.iter().map(|s| gen(*s.clone(), indent+1)).collect::<Vec<_>>().join(",\n"))
        },
        at::Call{program_name, using_parameters, giving_parameter, on_exception} => {
            let using = match using_parameters { 
                Some(params) => format!(", [{}]", params.iter().map(|p| gen(*p.clone(), 0)).collect::<Vec<_>>().join(", ")),
                None => String::new()
            };
            let giving = match giving_parameter { Some(g) => format!(" giving {}", gen(*g, 0)), None => String::new() };
            let exception = match on_exception { Some(e) => format!(" on exception {}", gen(*e, 0)), None => String::new() };
            format!("{}call({}){}{}{})", c_indent, gen(*program_name, 0), using, giving, exception)
        },
        at::QualifiedName{data_name, qualifier_list} => {
            format!("{}{}#{}", c_indent, qualifier_list.join("#"), data_name)
        },
        at::Subscript{data_name, subscript_list} => {
            format!("{}{}[{}]", c_indent, data_name,
                subscript_list.iter().map(|s| gen(*s.clone(), 0)).collect::<Vec<_>>().join(", "))
        },
        at::Figurative{figurative_type} => {
            match figurative_type.as_str() {
                "SPACES" => format!("{}\" \"", c_indent),
                "ZEROS" | "ZEROES" => format!("{}0", c_indent),
                "HIGH-VALUES" => format!("{}<<255>>", c_indent),
                "LOW-VALUES" => format!("{}<<0>>", c_indent),
                _ => format!("{}{}", c_indent, figurative_type)
            }
        },
        at::ClassCondition{data_item, class_name} => {
            format!("{}is_{}({})", c_indent, class_name.to_lowercase(), gen(*data_item, 0))
        },
        at::SignCondition{data_item, sign} => {
            let op = match sign.as_str() {
                "POSITIVE" => "> 0",
                "NEGATIVE" => "< 0",
                "ZERO" => "=:= 0",
                _ => sign.as_str()
            };
            format!("{}{} {}", c_indent, gen(*data_item, 0), op)
        },
        at::OccursClause{min_occurs, max_occurs, depending_on, indexed_by} => {
            let min = match min_occurs { Some(m) => format!("min: {}, ", m), None => String::new() };
            let depending = match depending_on { Some(d) => format!(", depending on: {}", d), None => String::new() };
            let indexed = match indexed_by { Some(i) => format!(", indexed by: {}", i.join(", ")), None => String::new() };
            format!("{}%% occurs {}max: {}{}{}", c_indent, min, max_occurs, depending, indexed)
        },
        at::HexLiteral{value} => {
            format!("{}<<16#{}>", c_indent, value)
        },
        at::UsageClause{usage_type} => {
            format!("{}%% usage {}", c_indent, usage_type)
        }
    }
}

pub fn generate(ast: at) -> String {
    gen(ast, 0)
}
