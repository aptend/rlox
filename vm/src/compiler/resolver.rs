struct Local {
    name: String,
    depth: Option<usize>,
}

pub struct Resolver {
    locals: Vec<Local>,
    cur_depth: usize,
}

impl std::default::Default for Resolver {
    fn default() -> Self {
        Resolver {
            // Every function has a reserved local variable position for VM use
            locals: vec![Local {
                name: "".to_string(),
                depth: Some(0),
            }],
            cur_depth: 0,
        }
    }
}

impl Resolver {
    pub fn is_local_ready(&self) -> bool {
        self.cur_depth > 0
    }

    pub fn begin_scope(&mut self) {
        self.cur_depth += 1;
    }

    pub fn end_scope(&mut self) -> usize {
        self.cur_depth -= 1;
        let mut pop_n = 0;
        while let Some(local) = self.locals.last() {
            if let Some(depth) = local.depth {
                if depth > self.cur_depth {
                    pop_n += 1;
                    self.locals.pop();
                } else {
                    break;
                }
            }
        }
        pop_n
    }

    /// Declare a local variable on stack.  
    /// Return true if successful, false if there is a duplicated name variable
    /// in the same scope.
    pub fn declare_variable(&mut self, name: &str) -> bool {
        // it is impossible to found an uninitialized variable when declaring
        // so it is safe to unwrap the local's depth
        if self
            .locals
            .iter()
            .rev()
            .take_while(|loc| loc.depth.unwrap() == self.cur_depth)
            .any(|loc| loc.name == name)
        {
            false
        } else {
            self.locals.push(Local {
                name: name.to_string(),
                depth: None, // not intialized yet
            });
            true
        }
    }

    pub fn resolve_variale(&self, name: &str) -> Option<usize> {
        if self.cur_depth == 0 {
            return None;
        }
        self.locals
            .iter()
            .rev()
            .position(|loc| loc.name == name)
            .map(|idx| self.locals.len() - 1 - idx)
    }

    // is_captured is useless because of Instruction::CloseUpvalue(n) rather
    // than Instruction::CloseUpvalue
    // pub fn mark_captured(&mut self, index: usize) {
    //     if let Some(local) = self.locals.get_mut(index) {
    //         local.is_captured = true;
    //     }
    // }

    pub fn mark_initialized(&mut self) {
        if self.cur_depth == 0 {
            // compile top-level recursive function will touch here.
            return;
        }
        self.locals.last_mut().unwrap().depth = Some(self.cur_depth);
    }
}
