use buscal::{evaluator, parser, type_checker, Span};

#[test]
fn test_evaluator_error() {
    #[derive(Debug)]
    struct TestCase<'a> {
        name: &'a str,
        program: String,
    }

    let tests = [TestCase {
        name: "cannot pass array to len directly",
        program: String::from(
            r###"
                len(["this is", "test"]);
            "###,
        ),
    }];

    for t in tests.iter() {
        println!("{}", t.name);
        let span = Span::new(&t.program);
        let parsed_statements = match parser::statements_finish(span) {
            Ok(stmts) => stmts,
            Err(e) => {
                panic!("Parse error: {e:?}\n{:?}", t.name);
            }
        };

        let mut tc_ctx = type_checker::Context::new();
        assert!(type_checker::type_check(&parsed_statements, &mut tc_ctx).is_ok());

        let mut frame = evaluator::StackFrame::new();
        let mut lines = vec![];
        lines.push("#!/usr/bin/env bash".to_string());
        lines.push("set -e".to_string());
        assert!(evaluator::eval_stmts(&parsed_statements, &mut frame, &mut lines, 0).is_err());
    }
}
