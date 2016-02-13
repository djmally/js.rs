#[macro_use]
mod macros;

use coerce::{AsBool,AsNumber};

use french_press::ScopeManager;
use js_types::binding::Binding;
use js_types::js_fn::JsFnStruct;
use js_types::js_var::{JsVar, JsType, JsPtrEnum};
use js_types::js_var::JsType::*;

use jsrs_parser::lalr::parse_Stmt;
use jsrs_common::ast::*;
use jsrs_common::ast::Exp::*;
use jsrs_common::ast::BinOp::*;
use jsrs_common::ast::Stmt::*;

pub fn eval_string(string: &str, state: &mut ScopeManager) -> JsVar {
    match parse_Stmt(string) {
        Ok(stmt) => {
            let (v, _, _) = eval_stmt(&stmt, state);
            v
        }
        Err(_) => panic!("parse error"),
    }
}

pub fn eval_stmt(s: &Stmt, mut state: &mut ScopeManager) -> (JsVar, Option<JsPtrEnum>, Option<(JsVar, Option<JsPtrEnum>)>) {
    match *s {
        Assign(ref var_string, ref exp) => {
            // TODO: this is a hack to return the value properly, which should be changed once we
            // stop using HashMap to store state.
            let (mut var, ptr) = eval_exp(exp, state);
            let cloned = var.clone();
            var.binding = Binding::new(var_string.clone());
            match state.alloc(var, ptr.clone()) {
                Ok(_) => (),
                e @ Err(_) => println!("{:?}", e),
            }
            (cloned, ptr, None)
        },
        BareExp(ref exp) => {
            let (v, p) = eval_exp(exp, &mut state);
            (v, p, None)
        }
        Decl(ref var_string, ref exp) => {
            let (mut var, ptr) = eval_exp(exp, state);
            var.binding = Binding::new(var_string.clone());
            // TODO: use value
            match state.alloc(var.clone(), ptr.clone()) {
                Ok(_) => (var, ptr, None),
                e @ Err(_) => panic!("{:?}", e),
            }
        },
        Empty => (JsVar::new(JsUndef), None, None),
        If(ref condition, ref if_block, ref else_block) => {
            let (v, _) = eval_exp(&condition, state);
            if v.as_bool() {
                eval_stmt(&*if_block, state)
            } else {
                if let Some(ref block) = *else_block {
                    eval_stmt(&*block, state)
                } else {
                    (JsVar::new(JsUndef), None, None)
                }
            }
        },
        Ret(ref e) => {
            let (v, p) = eval_exp(&e, &mut state);
            (v.clone(), p.clone(), Some((v, p)))
        }
        Seq(ref s1, ref s2) => {
            let _exp = eval_stmt(&*s1, &mut state);
            eval_stmt(&*s2, &mut state)
        },
        While(ref condition, ref block) => {
            let mut ret_val = None;
            loop {
                let (v, _)  = eval_exp(&condition, state);
                if v.as_bool() {
                    let (_, _, v) = eval_stmt(&*block, state);
                    ret_val = v;
                } else {
                    return (JsVar::new(JsUndef), None, ret_val);
                }
            }
        }
    }
}

pub fn eval_exp(e: &Exp, mut state: &mut ScopeManager) -> (JsVar, Option<JsPtrEnum>) {
    match e {
        &BinExp(ref e1, ref op, ref e2) => {
            let (val1, ptr1) = eval_exp(e1, state);
            let (val2, ptr2) = eval_exp(e2, state);

            match *op {
                And => if val1.as_bool() {
                    (val2, ptr2)
                } else {
                    (val1, ptr1)
                },
                Or  => if val1.as_bool() {
                    (val1, ptr1)
                } else {
                    (val2, ptr2)
                },
                Ge  => (JsVar::new(JsBool(val1.as_number() >= val2.as_number())), None),
                Gt  => (JsVar::new(JsBool(val1.as_number() > val2.as_number())), None),
                Le  => (JsVar::new(JsBool(val1.as_number() <= val2.as_number())), None),
                Lt  => (JsVar::new(JsBool(val1.as_number() < val2.as_number())), None),
                Neq => (JsVar::new(JsBool(val1.as_number() != val2.as_number())), None),
                Eql => (JsVar::new(JsBool(val1.as_number() == val2.as_number())), None),

                Minus => (JsVar::new(JsNum(val1.as_number() - val2.as_number())), None),
                Plus  => (JsVar::new(JsNum(val1.as_number() + val2.as_number())), None),
                Slash => (JsVar::new(JsNum(val1.as_number() / val2.as_number())), None),
                Star  => (JsVar::new(JsNum(val1.as_number() * val2.as_number())), None),
            }
        }
        &Bool(b) => (JsVar::new(JsBool(b)), None),
        &Call(ref fun_name, ref arg_exps) => {
            let (fun_binding, ptr) = eval_exp(fun_name, state);

            let mut args = Vec::new();

            for exp in arg_exps {
                args.push(eval_exp(exp, state));
            }

            let js_fn_struct = match ptr {
                Some(JsPtrEnum::JsFn(jfs)) => jfs,
                Some(_) => panic!("Invalid call object."),
                None => match state.load(&fun_binding.binding) {
                    Ok((_, Some(JsPtrEnum::JsFn(jfs)))) => jfs,
                    Ok(_) => panic!("Invalid call object."),
                    Err(_) => panic!("ReferenceError: {} is not defined")
                }
            };

            state.push_scope(js_fn_struct.id);

            for param in js_fn_struct.params.iter() {
                let (mut arg, ptr) = if args.is_empty() {
                    (JsVar::new(JsUndef), None)
                } else {
                    args.remove(0)
                };
                arg.binding = Binding::new(param.to_owned());
                state.alloc(arg, ptr).expect("Unable to store function argument in scope");
            }

            let (_, _, v) = eval_stmt(&js_fn_struct.stmt, state);

            // Should we yield here? Not sure, so for now it doesn't
            state.pop_scope(false);
            v.unwrap_or((JsVar::new(JsUndef), None))
        },
        &Defun(ref opt_binding, ref params, ref body) => {
            let id = state.add_scope().expect("Unable to add new scope");
            let js_fun = JsFnStruct::new(opt_binding, params, &**body, id);
            let mut var = JsVar::new(JsPtr);
            var.binding = if let &Some(ref s) = opt_binding {
                Binding::new(s.to_owned())
            } else {
                Binding::anon()
            };
            if let Err(_) = state.alloc(var, Some(JsPtrEnum::JsFn(js_fun.clone()))) {
                panic!("error storing function into state");
            }
            (JsVar::new(JsPtr), Some(JsPtrEnum::JsFn(js_fun)))
        },
        &Float(f) => (JsVar::new(JsType::JsNum(f)), None),
        &InstanceVar(..) => unimplemented!(),
        &Method(..) => unimplemented!(),
        &Null=> (JsVar::new(JsNull), None),
        &Neg(ref exp) => {
            let (v, _) =  eval_exp(exp, state);
            (JsVar::new(JsNum(-v.as_number())), None)
        },
        &Pos(ref exp) => {
            let (v, _) =  eval_exp(exp, state);
            (JsVar::new(JsNum(v.as_number())), None)
        },
        &PostDec(ref exp) => eval_float_post_op!(exp, f, f - 1.0, state),
        &PostInc(ref exp) => eval_float_post_op!(exp, f, f + 1.0, state),
        &PreDec(ref exp)  => eval_float_pre_op!(exp, f, f - 1.0, state),
        &PreInc(ref exp)  => eval_float_pre_op!(exp, f, f + 1.0, state),

        &NewObject(_, _) => unimplemented!(),
        &Object(_) => unimplemented!(),
        &Undefined => (JsVar::new(JsUndef), None),
        &Var(ref var_binding) => {
            match state.load(&Binding::new(var_binding.clone())) {
                Ok((var, ptr)) => (var, ptr),
                _ => panic!(format!("ReferenceError: {} is not defined", var_binding))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::hash_set::HashSet;
    use french_press::init_gc;
    use js_types::js_var::JsType;
    use js_types::binding::Binding;

    #[test]
    fn test_eval_literals() {
        let mut state = init_gc();
        assert_eq!(JsType::JsNum(5.0f64), eval_string("5.0;\n", &mut state).t);
        assert_eq!(JsType::JsNum(0.0f64), eval_string("0.0;\n", &mut state).t);
        assert_eq!(JsType::JsUndef, eval_string("undefined;\n", &mut state).t);
    }

    //// TODO: handle `var` and no `var` separately
    //#[test]
    //fn test_store_state() {
    //    let mut state = HashMap::new();
    //    assert_eq!(JsUndefined, eval_string("var a = 1;\n", &mut state));
    //    assert_eq!(JsNumber(2.0f64), eval_string("a = 2;\n", &mut state));
    //    assert_eq!(JsUndefined, eval_string("var b = 3;\n", &mut state));
    //    assert_eq!(JsNumber(4.0f64), eval_string("c = 4;\n", &mut state));
    //}

    #[test]
    fn test_inc_dec() {
        let mut state = init_gc();
        //assert_eq!(JsType::JsNum(1.0f64), eval_string("var a = 1;\n", &mut state).t);
        //assert_eq!(&JsType::JsNum(1.0), state.load(&Binding::new("a")).unwrap());

        //assert_eq!(JsType::JsNum(1.0f64), eval_string("a++;\n", &mut state));
        //assert_eq!(&JsNumber(2.0f64), state.get("a").unwrap());

        //assert_eq!(JsNumber(3.0f64), eval_string("++a;\n", &mut state));
        //assert_eq!(&JsNumber(3.0f64), state.get("a").unwrap());

        //assert_eq!(JsNumber(3.0f64), eval_string("a--;\n", &mut state));
        //assert_eq!(&JsNumber(2.0f64), state.get("a").unwrap());

        //assert_eq!(JsNumber(1.0f64), eval_string("--a;\n", &mut state));
        //assert_eq!(&JsNumber(1.0f64), state.get("a").unwrap());
    }

    #[test]
    fn test_binexp() {
        let mut state = init_gc();
        assert_eq!(JsType::JsNum(6.0f64),  eval_string("2.0 + 4.0;\n", &mut state).t);
        assert_eq!(JsType::JsNum(0.5f64),  eval_string("2.0 / 4.0;\n", &mut state).t);
        assert_eq!(JsType::JsNum(-2.0f64), eval_string("2.0 - 4.0;\n", &mut state).t);
        assert_eq!(JsType::JsNum(8.0f64),  eval_string("2.0 * 4.0;\n", &mut state).t);
    }
}
