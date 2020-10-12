use crate::ast::*;
use crate::interpreter::Interpreter;
use crate::parser::SyntaxError;
use crate::scanner::Token;

use std::collections::hash_map::{Entry, HashMap};

type ParseResult<T> = Result<T, SyntaxError>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
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
    scopes: Vec<HashMap<&'a str, bool>>,
    interpreter: &'a mut Interpreter,
    current_function: FunctionType,
    // Could have moved BreakOutside here. It is trival
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Resolver {
            scopes: Vec::new(),
            interpreter,
            current_function: FunctionType::None,
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
        let mut errors = Vec::new();
        for stmt in stmts {
            if let Err(e) = stmt.resolve(self) {
                errors.push(e);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        // unwrap unbalanced begin and end
        self.scopes.pop().unwrap();
    }

    fn declare(&mut self, name: &'a Token) -> ParseResult<()> {
        if let Some(scope) = self.scopes.last_mut() {
            let name_str: &str = name.as_str().unwrap();
            match scope.entry(name_str) {
                Entry::Occupied(_) => {
                    return Err(SyntaxError::AlreadyExistVarInScope(Box::new(
                        name.clone(),
                    )))
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

    fn resolve_function(&mut self, stmt: &'a FunctionStmt) -> ParseResult<()> {
        let enclosing_function = self.current_function;
        self.current_function = FunctionType::Function;
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
                resolver.resolve_function(f)
            }

            Stmt::Class(c) => {
                resolver.declare(&c.name)?;
                resolver.define(&c.name);
                Ok(())
            }

            // the following match arms exist to lead us in the maze of
            // syntax tree
            Stmt::Print(p) => p.resolve(resolver),
            Stmt::Expression(e) => e.resolve(resolver),
            Stmt::Return(r) => {
                if resolver.current_function == FunctionType::None {
                    Err(SyntaxError::ReturnOutside(Box::new(r.ret_tk.clone())))
                } else {
                    r.value.resolve(resolver)
                }
            }
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
                        return Err(SyntaxError::ReadLocalInitializer(
                            Box::new(v.name.clone()),
                        ));
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

            // the following match arms exist to lead us in the maze of
            // syntax tree
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
