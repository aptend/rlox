use crate::ast::*;
use crate::interpreter::Interpreter;
use crate::parser::SyntaxError;
use crate::scanner::Token;

use std::collections::hash_map::{Entry, HashMap};

type ParseResult<T> = Result<T, SyntaxError>;

//TODO: [IMPORTANT] Review if it is neccesary to return from error? Understand panic mode

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
    Method,
    Initializer,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum ClassType {
    None,
    Class,
}

// Let me try lifetime approach for Resolver

// Resolver.scopes' Key is refer to the built syntax tree(Vec<Stmt>)
//
// Actually, the complete declaration will be:
// pub struct Resolver<'a, 'i> {
//     scopes: Vec<HashMap<&'a str, bool>>,
//     interpreter: &'i mut Interpreter,
// }
//
// but my purpose is decalaring a lifetime relationship between HashMap
// and the built syntax, there is nothing to do with &mut Interpreter, so
// only one lifetime mark is used and compiler handles all the left.
pub struct Resolver<'a> {
    errors: Vec<SyntaxError>,
    scopes: Vec<HashMap<&'a str, bool>>,
    interpreter: &'a mut Interpreter,
    current_function: FunctionType,
    current_class: ClassType,
    // Could have moved BreakOutside here. It is trival
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Resolver {
            errors: Vec::new(),
            scopes: Vec::new(),
            interpreter,
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    // Param 'stmts' is a reference of the built syntax, marked by 's
    // We claim that the built syntax will outlive the Resolver, then we are
    // able to refer data from the syntax tree in method 'declare' or 'define'
    pub fn resolve<'s>(
        &mut self,
        stmts: &'s [Stmt],
    ) -> Result<(), Vec<SyntaxError>>
    where
        's: 'a,
    {
        for stmt in stmts {
            if let Err(e) = stmt.resolve(self) {
                self.errors.push(e);
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        // unwrap unbalanced begin and end
        self.scopes.pop().unwrap();
    }

    // we start resolve from global environment without a corresponding scope
    // which means duplicated decalarations of variable / function / class
    // are allowed, and the last one wins.
    fn declare(&mut self, name: &'a Token) -> ParseResult<()> {
        if let Some(scope) = self.scopes.last_mut() {
            let name_str: &str = name.as_str().unwrap();
            match scope.entry(name_str) {
                Entry::Occupied(_) => {
                    self.errors.push(SyntaxError::AlreadyExistInScope(
                        Box::new(name.clone()),
                    ));
                }
                Entry::Vacant(e) => {
                    e.insert(false);
                }
            }
        }
        Ok(())
    }

    fn define(&mut self, name: &'a Token) {
        if let Some(scope) = self.scopes.last_mut() {
            let name_str: &str = name.as_str().unwrap();
            scope.insert(name_str, true);
        }
    }

    // for 'this' and 'super'
    fn define_str(&mut self, name: &'static str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, true);
        }
    }

    // Resolver figures out how many hops to take during resovling a expression
    //
    // jlox stores these information in a HashMap<Expr, int>
    //
    // However, in rlox, ownership stands in our way to mirror this.
    // Therefore, we plan to use a dedicated expr_key as the key of hashmap.
    // First try, recording the expr's memory address as expr_key. But we
    // have to introduce something like Pin to prevent moving expr in memory.
    // It is a bit annoying in treewalk implementation.
    //
    // So we add an expr_key field to VariableExpr and AssignmentExpr, which will
    // be resolved during interpreting. This field will be filled with
    // increasing number during parsing.
    fn resolve_local(&mut self, expr_key: u64, name: &'a str) {
        for (hops, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                self.interpreter.add_resolution(expr_key, hops);
                return;
            }
        }
    }

    fn resolve_function(
        &mut self,
        stmt: &'a FunctionStmt,
        fun_type: FunctionType,
    ) -> ParseResult<()> {
        let enclosing_function = self.current_function;
        self.current_function = fun_type;
        self.begin_scope();
        for param in &stmt.params {
            self.declare(param)?;
            self.define(param);
        }
        // resolve body manually, avoiding begin_scope again.
        for b in &stmt.body.stmts {
            b.resolve(self)?;
        }
        self.end_scope();
        self.current_function = enclosing_function;
        Ok(())
    }
}

pub trait Resolve {
    fn resolve<'s, 'a>(
        &'s self,
        resolver: &mut Resolver<'a>,
    ) -> ParseResult<()>
    where
        's: 'a;
}

impl Resolve for Stmt {
    fn resolve<'s, 'a>(&'s self, resolver: &mut Resolver<'a>) -> ParseResult<()>
    where
        's: 'a,
    {
        match self {
            // comes to a block, we need to expand a new level scope to track
            // the following statements
            Stmt::Block(b) => {
                resolver.begin_scope();
                for stmt in &b.stmts {
                    stmt.resolve(resolver)?;
                }
                resolver.end_scope();
                Ok(())
            }
            // comes to a variable declaration, it modifies the current
            // environment, and we need to track its action to help us
            // resolving the following variable and assignment expression
            //
            // declare it first, we can return an Error in some cases,
            // for instance, when initializer expr contains
            // the same name variable, like this:
            // var a = "outer";
            // {
            //   var a = a;  // SyntraxError
            // }
            Stmt::Var(v) => {
                resolver.declare(&v.name)?;
                v.init.resolve(resolver)?;
                resolver.define(&v.name);
                Ok(())
            }

            Stmt::Function(f) => {
                resolver.declare(&f.name)?;
                resolver.define(&f.name);
                resolver.resolve_function(f, FunctionType::Function)
            }

            Stmt::Class(c) => {
                let current_class = resolver.current_class;
                resolver.current_class = ClassType::Class;
                resolver.declare(&c.name)?;
                resolver.define(&c.name);
                // mock the bound environment, where 'this' lives
                resolver.begin_scope();
                resolver.define_str("this");
                for stmt in &c.methods {
                    // No declare & define method's name here, because
                    // it is stored in LoxClass instead of environment.
                    //
                    // With defining name here, duplicated method declarations
                    // will be denied. I think this is helpful in large
                    // codebase
                    //
                    // What is the cost? In the following code snippet
                    //
                    // fun say_hi { print "hi"; }
                    // class Foo {
                    //    say_hi() { print "hi, foo"; }
                    //    say_any() { say_hi(); }
                    // }
                    //
                    // calls foo.say_any() will find say_hi in bound env
                    // where no 'say_hi' lives, during runtime. Then it
                    // will search in its enclosing env, along up to the global.
                    //
                    // It's acceptable.

                    let fun_type = if stmt.name.as_str().unwrap() == "init" {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };
                    resolver.declare(&stmt.name)?;
                    resolver.define(&stmt.name);
                    resolver.resolve_function(stmt, fun_type)?;
                }
                resolver.end_scope();
                resolver.current_class = current_class;
                Ok(())
            }

            // the following match arms exist to lead us in the maze of
            // syntax tree
            Stmt::Print(p) => p.resolve(resolver),
            Stmt::Expression(e) => e.resolve(resolver),
            Stmt::Return(r) => match resolver.current_function {
                FunctionType::None => {
                    resolver.errors.push(SyntaxError::ReturnOutside(Box::new(
                        r.ret_tk.clone(),
                    )));
                    Ok(())
                }
                FunctionType::Initializer if Expr::is_nil(&r.value) => {
                    resolver.errors.push(SyntaxError::ReturnValueInInit(
                        Box::new(r.ret_tk.clone()),
                    ));
                    Ok(())
                }
                _ => r.value.resolve(resolver),
            },
            Stmt::While(w) => {
                w.cond.resolve(resolver)?;
                w.body.resolve(resolver)
            }
            Stmt::If(i) => {
                i.cond.resolve(resolver)?;
                i.taken.resolve(resolver)?;
                if let Some(branch) = &i.no_token {
                    branch.resolve(resolver)?;
                }
                Ok(())
            }
            Stmt::Break => Ok(()),
        }
    }
}

impl Resolve for Expr {
    fn resolve<'s, 'a>(&'s self, resolver: &mut Resolver<'a>) -> ParseResult<()>
    where
        's: 'a,
    {
        match self {
            Expr::Variable(v) => {
                let name: &str = v.name.as_str().unwrap();
                if let Some(scope) = resolver.scopes.last() {
                    if let Some(false) = scope.get(name) {
                        resolver.errors.push(
                            SyntaxError::ReadLocalInitializer(Box::new(
                                v.name.clone(),
                            )),
                        );
                        return Ok(());
                    }
                }
                resolver.resolve_local(v.expr_key, name);
                Ok(())
            }
            Expr::Assign(a) => {
                a.value.resolve(resolver)?;
                resolver.resolve_local(a.expr_key, a.name.as_str().unwrap());
                Ok(())
            }

            Expr::This(t) => {
                if resolver.current_class == ClassType::None {
                    resolver.errors.push(SyntaxError::ThisOutside(Box::new(
                        t.this_tk.clone(),
                    )));
                } else {
                    resolver.resolve_local(t.expr_key, "this");
                }
                Ok(())
            }

            // the following match arms exist to lead us in the maze of
            // syntax tree
            Expr::Get(g) => g.object.resolve(resolver),
            Expr::Set(s) => {
                s.object.resolve(resolver)?;
                s.value.resolve(resolver)
            }
            Expr::Unary(u) => u.right.resolve(resolver),
            Expr::Binary(b) => {
                b.left.resolve(resolver)?;
                b.right.resolve(resolver)
            }
            Expr::Logical(l) => {
                l.left.resolve(resolver)?;
                l.right.resolve(resolver)
            }
            Expr::Call(c) => {
                c.callee.resolve(resolver)?;
                for arg in &c.arguments {
                    arg.resolve(resolver)?;
                }
                Ok(())
            }
            Expr::Grouping(g) => g.expr.resolve(resolver),
            Expr::Literal(_) => Ok(()),
        }
    }
}
