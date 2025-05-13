use log::error;

use buscal::{evaluator, parser, type_checker, Span};

#[test]
fn test_parse_error() {
    #[derive(Debug)]
    struct TestCase<'a> {
        name: &'a str,
        program: String,
    }

    let tests = [
        TestCase {
            name: "型宣言がない",
            program: String::from("var s = 1;"),
        },
        TestCase {
            name: "関数の返り値の型を宣言していない",
            program: String::from(
                r###"
              fn add(a: i64, b: i64) {
                return a + b, 0;
              }
              add(1, 3);
            "###,
            ),
        },
        TestCase {
            name: "missing {",
            program: String::from(
                r###"
                if 1 == 1
                  return "success", 0;
                } else {
                  return "fail", 1;
                }
            "###,
            ),
        },
        TestCase {
            name: "missing ',' in an array",
            program: String::from("var a1: array<str> = [\"ls\" \"-al\"]"),
        },
    ];

    for t in tests.iter() {
        let t = Span::new(&t.program);
        assert!(parser::statements_finish(t).is_err());
    }
}

#[test]
fn test_type_check_error() {
    #[derive(Debug)]
    struct TestCase<'a> {
        name: &'a str,
        program: String,
    }

    let tests = [
        TestCase {
            name: "assign i64 to the str var",
            program: String::from("var s: str = 1; "),
        },
        TestCase {
            name: "assign str to the i64 var",
            program: String::from("var i: i64 = \"test\";\n"),
        },
        TestCase {
            name: "pass str to the i64 arg",
            program: String::from(
                r###"
              fn add(a: i64, b: i64) -> i64 {
                return a + b;
              }
              add(1, "s");
            "###,
            ),
        },
        TestCase {
            name: "str + i64",
            program: String::from("\"1\" + 1;"),
        },
        TestCase {
            name: "str array contains i64",
            program: String::from(
                r###"
                var s1: i64 = 1;
                var a1: array::<str> = ["1", s1];
            "###,
            ),
        },
        TestCase {
            name: "i64 + str",
            program: String::from(
                r###"
                var a1: array::<i64> = [1, 2, 3];
                echo(1 + a1[0] + ")");
            "###,
            ),
        },
        TestCase {
            name: "void function returns i64",
            program: String::from(
                r###"
                fn add() -> void {
                  return 1;
                }
            "###,
            ),
        },
        TestCase {
            name: "variable scope",
            program: String::from(
                r###"
                fn f1() -> i64 {
                  var s1: str = "this is ";
                  if s1 == "this is " {
                    var res: str = "response";
                  }

                  return res;
                }

                f1();
            "###,
            ),
        },
        TestCase {
            name: "should pass two arguments",
            program: String::from(
                r###"
                var s: str = "this
                is a
                pen";

                var array: array::<str> = [""];
                split(ref array);
                for i in 0 to 2 {
                  echo(format("{}: {}", i, array[i]));
                }
            "###,
            ),
        },
    ];

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

        assert!(type_checker::type_check(&parsed_statements, &mut tc_ctx).is_err());
    }
}
