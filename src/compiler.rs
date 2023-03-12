use std::rc::Rc;

use crate::{token::{Span, Token, TokenKind}, environment::{Environment, ConstantValue, FunctionMeta}, instructions::{Instruction, ParamKind}, nativefns::{self, NativeFunction}, symtable::SymTable, ast::{Ast, AstData}, object::Object, source::Source, RunArgs};

#[derive(Debug, Clone)]
struct LookAhead {
    token: Token,
    args: u8,
    // Position in bytecode
    position: u32,
}

#[derive(Clone)]
#[repr(u8)]
pub enum Function {
    User {
        meta_id: u32,
        span: Span,
        parameters: u8,
        empty: bool,
    },
    Native {
        identifier: &'static str,
        param_count: ParamKind,
        function: NativeFunction,
        has_return: bool,
    },
}

impl Function {
    fn is_empty(&self) -> bool {
        if let Function::User { meta_id: _, span: _, parameters: _, empty } = *self {
            empty
        } else {
            false
        }
    }

    fn mark_empty(&mut self) {
        if let Function::User { meta_id: _, span: _, parameters: _, ref mut empty } = * self {
            *empty = true;
        } 
    }
}

struct Symbols {
    table: Vec<Span>,
}

impl Symbols {
    pub fn new() -> Self {
        Self { table: Vec::new() }
    }

    pub fn find_or_add(&mut self, span: &Span) -> u16 {
        for (idx, item) in self.table.iter().enumerate() {
            if span.slice_source() == item.slice_source() {
                return idx as u16;
            }
        }

        self.table.push(span.clone());
        self.table.len() as u16 - 1
    }
}

pub struct Compiler {
    had_error: bool,
    source: Rc<Source>,
    args: RunArgs,
    unresolved: Vec<LookAhead>,
    table: SymTable,
    functable: Vec<Function>,
    symbols: Symbols,
}

pub struct CompilerErr {
    pub file_path: String,
    pub message: String,
}

impl Compiler {
    pub fn new(source: Rc<Source>, args: RunArgs) -> Self {
        Self {
            had_error: false,
            source,
            args,
            unresolved: Vec::new(),
            table: SymTable::new(),
            functable: Vec::new(),
            symbols: Symbols::new(),
        }
    }

    pub fn run(&mut self, program: Box<Ast>) -> Result<Box<Environment>, CompilerErr> {
        let mut env = Box::new(Environment::new());
        nativefns::register_native_functions(self, &mut env);

        self.program(&mut env, &program)?;

        if !self.args.script_mode {
            match self.find_function_str("main") {
                Some(func) => {
                    if let Function::User { meta_id, .. } = func {
                        env.add_call(*meta_id);
                    }
                }
                None => return Err(self.error("Cannot find function 'main'".into())),
            }
        }

        env.add_op(Instruction::Halt);

        // Try to resolve calls that were not during compilation
        self.resolve_function_calls(&mut env);

        if self.had_error {
            return Err(self.error("Error(s) occured".into()));
        }

        Ok(env)
    }

    pub fn add_fn(
        &mut self,
        env: &mut Box<Environment>,
        identifier: &'static str,
        param_count: ParamKind,
        has_return: bool,
        function: NativeFunction,
    ) {
        let function = Function::Native { 
            identifier,
            param_count,
            function,
            has_return,
        };
        // Add to function table
        self.functable.push(function.clone());

        // Add to environment
        env.add_constant_function(ConstantValue::Func(function));
    }

    fn error(&mut self, message: String) -> CompilerErr {
        CompilerErr { file_path: (*self.source.file_path).clone(), message }
    }

    #[inline]
    fn push_scope(&mut self) {
        self.table.new_scope();
    }

    #[inline]
    fn pop_scope(&mut self) {
        self.table.close_scope();
    }

    fn resolve_function_calls(&mut self, env: &mut Box<Environment>) {
        let mut unresolved = Vec::new();

        for lookahead in self.unresolved.iter() {
            match self.find_function(&lookahead.token.span) {
                Some(ref mut func) => {
                    // Cannot resolve native calls, since they're part of the compiler
                    if let Function::User { meta_id, empty, ..} = func {
                        // Generate the function if it is not empty
                        if *empty {
                            self.warning(format!(
                                    "Calling empty function '{}'",
                                    lookahead.token.span.slice_source()
                                ),
                                &lookahead.token
                            );
                        }

                        let func = &env.functions[*meta_id as usize];
                        // Correct function ID, but arity does not match
                        if func.arg_count != lookahead.args {
                            unresolved.push((lookahead.token.clone(), func.arg_count, lookahead.args));
                            continue;
                        }

                        u32::to_be_bytes(*meta_id)
                            .into_iter()
                            .enumerate()
                            .for_each(
                                |(i, b)| 
                                env.code[lookahead.position as usize + 1 + i] = b
                            );
                        }
                    }

                None => unresolved.push((lookahead.token.clone(), 0, lookahead.args)),
            }
        }

        // Identifiers that were still not found
        for (token, params, args) in unresolved {
            let id = token.span.slice_source().to_string();

            if params != args {
                self.error_no_exit(format!(
                    "Function '{id}' expected {params} argument(s), but received {args}",
                    ),
                    &token
                );
            } else {
                self.error_no_exit(format!(
                    "Function '{id}' does not exist",
                    ),
                    &token
                );
            }
        }
    }

    fn error_no_exit(&mut self, msg: String, token: &Token) {
        self.had_error = true;

        println!("{}", format!(
            "[\x1b[31mError\x1b[0m] {} '{}' [{}:{}]",
            msg,
            token.span.source.file_path,
            token.line,
            token.column,
        ));
    }

    fn warning(&self, msg: String, token: &Token) {
        println!("{}", format!(
            "[\x1b[33mWarning\x1b[0m] {} '{}' [{}:{}]",
            msg,
            token.span.source.file_path,
            token.line,
            token.column,
        ));
    }

    fn find_function_str(&self, id: &str) -> Option<&Function> {
        for func in &self.functable {
            match func {
                Function::User { meta_id: _, span,  .. } => {
                    if span.compare_str(id, &self.source.content) {
                        return Some(func);
                    }
                }
                Function::Native { identifier, .. } => {
                    if id == *identifier {
                        return Some(func);
                    }
                }
            }
        }

        None
    }

    #[inline]
    fn find_function(&self, span: &Span) -> Option<&Function> {
        self.find_function_str(span.slice_source())
    }

    fn register_local(&mut self, token: &Token, mutable: bool, func: Option<u32>) -> Option<usize> {
        let local = self.table.find_local(&token.span, false);

        match local {
            Some(local) => {
                // We aren't allowed to overwrite, it is an error
                self.error_no_exit(format!(
                        "Local with identifier '{}' already exists in scope",
                        local.identifier.slice_source(),
                    ),
                    token
                );
                None
            }

            None => Some(self.table.new_local(token.clone().span, mutable, func) as usize),
        }
    }

    fn register_function(
        &mut self,
        identifier: &Token,
        position: u32,
        parameters: &Option<Vec<Box<Ast>>>,
        env: &mut Box<Environment>,
    ) -> Result<(), CompilerErr>
    {
        let func = self.find_function(&identifier.span);

        // Function already exists
        if func.is_some() {
            self.error_no_exit(
                format!(
                    "Function with identifier '{}' already exists",
                    identifier.span.slice_source()
                ),
                &identifier
            );
            return Err(self.error(format!(
                "Function with identifier '{}' already exists",
                identifier.span.slice_source()
            )));
        }

        _ = self.register_local(&identifier, false, Some(env.functions.len() as u32));
        
        self.push_scope();
        self.evaluate_params(parameters, env)?;

        let param_count = match parameters {
            Some(ref p) => p.len() as u8,
            None => 0,
        };

        env.functions.push(FunctionMeta::new(
            identifier.span.slice_source().to_string(),
            param_count,
            position
        ));

        self.functable.push(Function::User {
            meta_id: env.functions.len() as u32 - 1,
            span: identifier.clone().span,
            parameters: param_count,
            empty: false
        });

        Ok(())
    }

    #[inline]
    fn evaluate_params(
        &mut self,
        parameters: &Option<Vec<Box<Ast>>>,
        env: &mut Box<Environment>
    ) -> Result<(), CompilerErr> {
        // Register locals from parameters
        if let Some(ref params) = parameters {
            for (idx, param) in params.iter().enumerate() {
                if let AstData::Parameter { type_name } = &param.data {
                    _ = self.register_local(&param.token, false, None);
    
                    if let Some(type_name) = type_name {
                        env.add_op(Instruction::GetLocal);
                        env.add_opb(0);
                        env.add_opb(idx as u8);
                        self.check_valid_type(env, &type_name, true)?;
                    }
                }
            }
        }

        Ok(())
    }

    fn mark_function_empty(&mut self, id: &Span) {
        for func in &mut self.functable {
            // Cannot mark native functions as empty
            if let Function::User { span, .. } = func {
                if span.compare(&id, &self.source.content) {
                    func.mark_empty();
                }
            }
        }
    }

    fn function_call(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::FunctionCall { lhs, arguments } = &node.data {
            let (identifier, anonymous) = match lhs.data {
                // FIXME: Change how identifier works, now that we visit lhs
                AstData::Identifier => {
                    if let None = self.find_function(&lhs.token.span) {
                        match self.table.find_local(&lhs.token.span, true) {
                            Some(local) => (Some(lhs.token.clone()), local.func.is_none()),
                            None => {
                                self.error_no_exit(format!(
                                        "Unknown identifier found '{}'",
                                        lhs.token.span.slice_source()
                                    ), 
                                    &lhs.token
                                );
                                (Some(lhs.token.clone()), false)
                            }
                        }
                    } else {
                        (Some(lhs.token.clone()), false)
                    }
                },
                AstData::Function { anonymous, .. } => {
                    self.visit(env, &lhs)?;
                    (Some(lhs.token.clone()), anonymous)
                },
                _ => {
                    self.visit(env, &lhs)?;
                    (None, false)
                },
            };
            
            let arg_count = arguments.len() as u8;
            let mut fnerr: Option<(String, u8)> = None;

            for arg in arguments {
                self.visit(env, &arg)?;
            }

            if anonymous {
                if let Some(local) = self.table.find_local(&lhs.token.span, true) {
                    env.add_local(
                        if local.is_global() {Instruction::GetGlobal} else {Instruction::GetLocal},
                        local.position
                    );
                }
                env.add_local_call(arg_count);
                return Ok(());
            }

            if let Some(identifier) = identifier {
                match self.find_function(&identifier.span) {
                    Some(func) => {
                        if !func.is_empty() {
                            // Only generate the call if the function is not empty
                            match func {
                                Function::User { meta_id, span, ..} => {
                                    let func = &env.functions[*meta_id as usize];
                                    let id = span.slice_source().to_string();
    
                                    if func.arg_count != arg_count {
                                        fnerr = Some((id, func.arg_count));
                                    }
        
                                    env.add_call(*meta_id);
                                }
                                Function::Native { identifier, param_count, .. } => {
                                    if let ParamKind::Count(c) = param_count {
                                        if *c != arg_count {
                                            fnerr = Some((identifier.to_string(), *c));
                                        }
        
                                        env.add_call_native(
                                            *c,
                                            env.find_constant_func_loc(&identifier) as u32,
                                        );
                                    } else {
                                        // Add call with N arguments
                                        env.add_call_native(
                                            arg_count,
                                            env.find_constant_func_loc(&identifier) as u32,
                                        );
                                    }
                                }
                            }
                        } else {
                            let token = identifier.clone();
                            self.warning(format!(
                                    "Calling empty function '{}'",
                                    token.span.slice_source()
                                ),
                                &identifier
                            );
                        }
                    }
        
                    None => {
                        if let Some(_) = self.table.find_local(&identifier.clone().span, true) {
                            env.add_local_call(arg_count);
                        } else {
                            // Push the call to a stack of unresolved calls
                            // They will be filled in at the end, if they exist
                            let position = env.op_here() as u32;
                            env.add_call(0);
            
                            self.unresolved.push(LookAhead {
                                token: identifier.clone(),
                                args: arg_count,
                                position,
                            });
        
                        }
                    }
                }

                // Display error if it occured
                if fnerr.is_some() {
                    let fnerr = fnerr.unwrap();
                    self.error_no_exit(format!(
                            "Function '{}' expected {} argument(s), but received {arg_count}",
                            fnerr.0, fnerr.1,
                        ),
                        &identifier
                    );
                }
            } else {
                // Assume another call
                env.add_local_call(arg_count);
            }
        }
        
        Ok(())
    }

    fn literal(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        let lexeme = &node.token.span;

        match node.token.kind {
            TokenKind::Number => {
                env.add_constant(Object::Number(
                    self.source.content[lexeme.start..lexeme.start + lexeme.len]
                        .parse::<f32>()
                        .unwrap()
                ));
            }

            TokenKind::String => {
                env.add_constant(Object::String(
                    String::from(
                        &self.source.content[lexeme.start..lexeme.start + lexeme.len]
                    )
                ));
            }

            TokenKind::True =>  env.add_op(Instruction::True),
            TokenKind::False => env.add_op(Instruction::False),
            TokenKind::None =>  env.add_op(Instruction::None),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn symbol(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        let identifier = &node.token.span;
        let value = self.symbols.find_or_add(identifier);
        env.add_symbol(value);

        Ok(())
    }

    fn if_statement(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::IfStatement { condition, true_body, false_body } = &node.data {
            self.visit(env, condition)?;
    
            let before_block = env.add_jump_op(true);
            self.visit(env, true_body)?;
            
            if let Some(false_body) = false_body {
                let true_block = env.add_jump_op(false);
                env.patch_jump_op(before_block);
                self.visit(env, false_body)?;
                env.patch_jump_op(true_block);
            } else {
                env.patch_jump_op(before_block);
            }
        }
        Ok(())
    }

    fn trailing_if(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::TrailingIfStatement { statement, condition } = &node.data {
            self.visit(env, condition)?;
    
            let before_block = env.add_jump_op(true);
            self.visit(env, statement)?;
            env.patch_jump_op(before_block);
        }
        Ok(())
    }

    fn switch_case(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(bool, u32), CompilerErr> {
        if let AstData::SwitchCase { case, body } = &node.data {
            if let Some(case) = case {
                env.add_op(Instruction::Peek);
                self.visit(env, case)?;
                env.add_op(Instruction::EqualEqual);

                let jmp = env.add_jump_op(true);
                self.visit(env, body)?;
                let done_jmp = env.add_jump_op(false);
                env.patch_jump_op(jmp);

                return Ok((false, done_jmp));
            } else {
                self.visit(env, body)?;
                return Ok((true, 0));
            }
        }

        unreachable!();
    }

    fn switch_statement(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::SwitchStatement { condition, cases } = &node.data {
            let mut has_default = false;
            self.visit(env, condition)?;

            let mut end_locs = Vec::new();
            let mut default_case = None;

            for switch_case in cases {
                
                if let AstData::SwitchCase { case, .. } = &switch_case.data {
                    if let None = case {
                        default_case = Some(switch_case);
                        continue;
                    }
                }

                if let Ok((is_else, loc)) = self.switch_case(env, switch_case) {
                    if has_default && is_else {
                        self.error_no_exit(
                                "Trying to add multiple default branches to switch case".into()
                            ,
                            &node.token
                        );
                        continue;
                    }
                    if is_else {
                        has_default = true;
                        continue;
                    }
                    end_locs.push(loc);
                }
            }

            // Pop condition value
            env.add_op(Instruction::Pop);

            if let Some(default_case) = default_case {
                _ = self.switch_case(env, default_case);
            }

            for end in end_locs {
                env.patch_jump_op(end);
            }
        }

        Ok(())
    }

    fn for_statement(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::ForStatement { variable, condition, increment, body } = &node.data {
            self.push_scope();
    
            if let Some(var_decl) = variable {
                self.visit(env, &var_decl)?;
            }
    
            // Evaluate condition
            let start = env.op_here() as u32;
            self.visit(env, condition)?;
            
            let before_block = env.add_jump_op(true);
            match &body.data {
                AstData::Body(_) => self.body(env, body, false)?,
                _ => self.visit(env, body)?,
            }
    
            if let Some(increment) = increment {
                self.visit(env, increment)?;
            }
            
            // Return back before the condition to re-evaluate
            let jmp = env.add_jump_op(false);
            env.patch_jump_op_to(jmp as usize, start);
            env.patch_jump_op(before_block);
            
            self.pop_scope();
        }

        Ok(())
    }

    fn do_while_statement(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::DoWhileStatement { body, condition } = &node.data {
            let code_here = env.op_here();
            self.visit(env, &body)?;
            self.visit(env, &condition)?;
    
            let not = env.add_jump_op(true);
            let loc = env.add_jump_op(false);
            env.patch_jump_op_to(loc as usize, code_here as u32);
            env.patch_jump_op(not);
        }

        Ok(())
    }

    fn identifier(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) {
        let identifier = &node.token;

        match self.table.find_local(&identifier.span, true) {
            Some(local) => {
                if let Some(func) = &local.func {
                    env.add_get_fn(*func);
                } else {
                    env.add_local(
                        if local.is_global() { Instruction::GetGlobal } else { Instruction::GetLocal },
                        local.position
                    );
                }
            },

            None => self.error_no_exit(format!(
                    "Identifier '{}' does not exist in the current context",
                    identifier.span.slice_source(),
                ),
                &identifier.clone()
            ),
        }
    }

    fn binary_op(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::BinaryOp { lhs, rhs } = &node.data {
            self.visit(env, &lhs)?;
            self.visit(env, &rhs)?;

            match node.token.kind {
                TokenKind::Plus =>          env.add_op(Instruction::Add),
                TokenKind::Minus =>         env.add_op(Instruction::Sub),
                TokenKind::Star =>          env.add_op(Instruction::Mul),
                TokenKind::Slash =>         env.add_op(Instruction::Div),
                TokenKind::EqualEqual =>    env.add_op(Instruction::EqualEqual),
                TokenKind::NotEqual =>      env.add_op(Instruction::NotEqual),
                _ => unreachable!("{}", node.token.span.slice_source()),
            }
        }

        Ok(())
    }

    fn unary_op(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::UnaryOp { rhs } = &node.data {
            self.visit(env, &rhs)?;

            match node.token.kind {
                TokenKind::Minus | TokenKind::Bang => env.add_op(Instruction::Negate),
                _ => unreachable!(),
            }
        }

        Ok(())
    }

    fn logical_op(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::LogicalOp { lhs, rhs } = &node.data {
            self.visit(env, &lhs)?;
            self.visit(env, &rhs)?;

            match node.token.kind {
                TokenKind::Greater =>           env.add_op(Instruction::Greater),
                TokenKind::GreaterEqual =>      env.add_op(Instruction::GreaterEqual),
                TokenKind::Less =>              env.add_op(Instruction::Less),
                TokenKind::LessEqual =>         env.add_op(Instruction::LessEqual),
                _ => unreachable!(),
            }
        }

        Ok(())
    }
    
    fn check_valid_type(
        &mut self,
        env: &mut Box<Environment>,
        type_id: &Token,
        is_asrt: bool
    ) -> Result<(), CompilerErr> {
        let type_name = type_id.span.slice_source();
        match Compiler::check_type(type_name) {
            Some(id) => {
                if is_asrt {
                    env.add_type_check_asrt(id);
                } else {
                    env.add_type_check(id);
                }
            },
            None => {
                self.error_no_exit(
                    format!("Invalid type name in type assert '{type_name}'"),
                    &type_id
                );
            }
        }
        Ok(())
    }

    fn list_literal(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::ListLiteral(values) = &node.data {
            for arg in values {
                self.visit(env, &arg)?;
            }
    
            env.add_op(Instruction::BuildList);
            env.add_opb(values.len() as u8);
        }

        Ok(())
    }

    fn map_literal(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::MapLiteral(values) = &node.data {
            for (identifier, expr) in values {
                env.add_constant(Object::String(identifier.span.slice_source().to_string()));

                if let Some(expr) = expr {
                    self.visit(env, expr)?;
                } else {
                    if let Some(local) = self.table.find_local(&identifier.span, true) {
                        env.add_local(
                            if local.is_global() {Instruction::GetGlobal} else {Instruction::GetLocal},
                            local.position
                        );
                    } else {
                        self.error_no_exit(format!(
                                "Cannot find identifier '{}' to construct map literal",
                                identifier.span.slice_source()
                            ),
                            identifier
                        )
                    }
                }
            }
    
            env.add_op(Instruction::BuildMap);
            env.add_opb(values.len() as u8);
        }

        Ok(())
    }

    fn var_declaration(&mut self, env: &mut Box<Environment>, var_decl: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::VarDeclaration { is_mutable, expression } = &var_decl.data {
            let identifier = &var_decl.token;
            let local = self.register_local(identifier, *is_mutable, None).unwrap();

            if self.table.is_global() {
                // Check mutable
                if self.args.no_global_mut && *is_mutable {
                    self.error_no_exit(
                        format!(
                            "Cannot declare mutable variable '{}' in global scope, when the 'no-mutable' flag is set",
                            identifier.span.slice_source()
                        ),
                        identifier
                    );
                }

                // Check global
                if self.args.no_global {
                    self.error_no_exit(
                        format!(
                            "Cannot declare variable '{}' in global scope, when the 'no-global' flag is set",
                            identifier.span.slice_source()
                        ),
                        identifier
                    );
                }
            }
    
            // Produce the expression
            match expression {
                Some(expr) => self.visit(env, expr)?,
                None => env.add_op(Instruction::None),
            }
    
            env.add_op(if self.table.is_global() {
                Instruction::SetGlobal
            } else {
                Instruction::SetLocal
            });
    
            (local as u16).to_be_bytes().into_iter().for_each(|b| env.add_opb(b));
        }

        Ok(())
    }

    fn var_declarations(&mut self, env: &mut Box<Environment>, var_decls: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::VarDeclarations(decls) = &var_decls.data {
            for decl in decls {
                self.var_declaration(env, decl)?;
            }
        }
        Ok(())
    }

    fn var_assign(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::VarAssign { lhs, expression } = &node.data {
            let identifier = &node.token;

            self.visit(env, &lhs)?;
            self.visit(env, &expression)?;

            match self.table.find_local(&identifier.span, true) {
                Some(local) => {
                    if !local.mutable {
                        self.error_no_exit(format!(
                                "Cannot re-assign an immutable value '{}'",
                                identifier.span.slice_source(),
                            ),
                            &identifier
                        );
                    } else {
                        env.add_local(if local.is_global()
                            { Instruction::SetGlobal }
                            else { Instruction::SetLocal },
                            local.position
                        );
                    }
                }
                None => {
                    self.error_no_exit(format!(
                            "Cannot assign to variable '{}' as it does not exist or is not in the correct context",
                            identifier.span.slice_source(),
                        ),
                        &identifier
                    );
                }
            }
        }

        Ok(())
    }

    fn var_assign_equal(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::VarAssignEqual { operator, lhs, expression } = &node.data {
            let identifier = &node.token;

            self.visit(env, &lhs)?;
            self.visit(env, &expression)?;

            match self.table.find_local(&identifier.span, true) {
                Some(local) => {
                    if !local.mutable {
                        self.error_no_exit(format!(
                                "Cannot re-assign an immutable value '{}'",
                                identifier.span.slice_source(),
                            ),
                            &identifier
                        );
                    } else {
                        match operator.kind {
                            TokenKind::PlusEqual    => env.add_op(Instruction::Add),
                            TokenKind::MinusEqual   => env.add_op(Instruction::Sub),
                            TokenKind::StarEqual    => env.add_op(Instruction::Mul),
                            TokenKind::SlashEqual   => env.add_op(Instruction::Div),
                            _ => unreachable!(),
                        }

                        env.add_local(if local.is_global()
                            { Instruction::SetGlobal }
                            else { Instruction::SetLocal },
                            local.position
                        );

                        // env.add_op(Instruction::Pop);
                    }
                }
                None => {
                    self.error_no_exit(format!(
                            "Cannot assign to variable '{}' as it does not exist or is not in the correct context",
                            identifier.span.slice_source(),
                        ),
                        &identifier
                    );
                }
            }
        }

        Ok(())
    }

    fn return_statement(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::Return(expr) = &node.data {
            match expr {
                Some(expr) => {
                    self.visit(env, expr)?;
                    env.add_op(Instruction::Return);
                }
                None => env.add_op(Instruction::ReturnNone),
            }
        }

        Ok(())
    }
    
    fn program(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        let old_source = Rc::clone(&self.source);

        if let AstData::Program { source, body } = &node.data {
            self.source = Rc::clone(&source);
            for node in body {
                self.visit(env, node)?;
            }
        }

        self.source = old_source;
        Ok(())
    }

    fn std_include(&mut self, _env: &mut Box<Environment>, _node: &Box<Ast>) -> Result<(), CompilerErr> {
        todo!("Std include in compiler");
    }

    fn body(&mut self, env: &mut Box<Environment>, node: &Box<Ast>, new_scope: bool) -> Result<(), CompilerErr> {
        if let AstData::Body(ref statements) = &node.data {
            if new_scope {
                self.push_scope();
                for node in statements {
                    self.visit(env, node)?;
                }
                self.pop_scope();
            } else {
                for node in statements {
                    self.visit(env, node)?;
                }
            }
        }

        Ok(())
    }

    fn type_check(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::TypeCheck { is_assert, expression } = &node.data {
            self.visit(env, expression)?;
            self.check_valid_type(env, &node.token, *is_assert)?;
        }
        Ok(())
    }

    fn function(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::Function { anonymous, parameters, body } = &node.data {
            let identifier = &node.token;
            
            let jmp = env.add_jump_op(false);
            let start_loc = env.op_here() as u32;
            
            if *anonymous {
                self.push_scope();
                self.table.mark_depth_limit();
                self.evaluate_params(parameters, env)?;
            } else {
                self.register_function(identifier, start_loc, parameters, env)?;
                if !self.table.is_global() {
                    self.table.mark_depth_limit();
                }
            }
            
            match &body.data {
                AstData::Return(_) => self.return_statement(env, body)?,
                AstData::Body(_) => self.body(env, body, false)?,
                _ => unreachable!(),
            }
            self.pop_scope();
            self.table.reset_mark();
            
            // Don't generate pointless returns
            if let AstData::Body(ref statements) = &body.data {
                if statements.len() == 0 {
                    self.mark_function_empty(&identifier.span);
                    env.add_op(Instruction::ReturnNone);
                } else if !matches!(statements.last().unwrap().data, AstData::Return(_)) {
                    env.add_op(Instruction::ReturnNone);
                }
            }
            
            env.patch_jump_op(jmp);

            if *anonymous {
                env.add_anon_fn(match *parameters {
                        Some(ref p) => p.len() as u8,
                        None => 0,
                    },
                    start_loc
                );
            }
        };
        
        Ok(())
    }

    fn expression_statement(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::ExpressionStatement(expr) = &node.data {
            self.visit(env, expr)?;
            env.add_op(Instruction::Pop);
        }

        Ok(())
    }

    fn get_property(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) ->Result<(), CompilerErr> {
        if let AstData::GetProperty { lhs } = &node.data {
            self.visit(env, lhs)?;
            env.add_constant(Object::String(node.token.span.slice_source().to_string()));
            env.add_op(Instruction::GetProperty);
        }
        Ok(())
    }

    fn set_property(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) ->Result<(), CompilerErr> {
        if let AstData::SetProperty { lhs, expression } = &node.data {
            match lhs.data {
                AstData::GetProperty { ref lhs } => self.visit(env, lhs)?,
                _ => self.visit(env, lhs)?,
            }
            env.add_constant(Object::String(node.token.span.slice_source().to_string()));
            self.visit(env, expression)?;
            env.add_op(Instruction::SetProperty);
        }
        Ok(())
    }

    fn index_getter(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::IndexGetter { expression, index } = &node.data {
            self.visit(env, expression)?;
            self.visit(env, index)?;
            env.add_op(Instruction::IndexGet);
        }
        Ok(())
    }

    fn index_setter(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        if let AstData::IndexSetter { expression, rhs } = &node.data {
            if let AstData::IndexGetter { expression, index } = &expression.data {
                self.visit(env, expression)?;
                self.visit(env, index)?;
            }
            
            self.visit(env, rhs)?;
            
            env.add_op(Instruction::IndexSet);
        }
        Ok(())
    }

    fn visit(&mut self, env: &mut Box<Environment>, node: &Box<Ast>) -> Result<(), CompilerErr> {
        match node.data {
            AstData::Literal => self.literal(env, node)?,
            AstData::SymbolLiteral => self.symbol(env, node)?,
            AstData::ListLiteral(_) => self.list_literal(env, node)?,
            AstData::MapLiteral(_) => self.map_literal(env, node)?,
            AstData::Identifier => self.identifier(env, node),
            
            AstData::BinaryOp {..} => self.binary_op(env, node)?,
            AstData::UnaryOp {..} => self.unary_op(env, node)?,
            AstData::LogicalOp {..} => self.logical_op(env, node)?,
            
            AstData::VarDeclaration {..} => self.var_declaration(env, node)?,
            AstData::VarDeclarations {..} => self.var_declarations(env, node)?,
            AstData::VarAssign {..} => self.var_assign(env, node)?,
            AstData::VarAssignEqual {..} => self.var_assign_equal(env, node)?,
            AstData::Return(_) => self.return_statement(env, node)?,
            AstData::Body(_) => self.body(env, node, false)?,
            AstData::TypeCheck {..} => self.type_check(env, node)?,

            AstData::Function {..} => self.function(env, node)?,
            AstData::FunctionCall {..} => self.function_call(env, node)?,
            AstData::IfStatement {..} => self.if_statement(env, node)?,
            AstData::ForStatement {..} => self.for_statement(env, node)?,
            AstData::DoWhileStatement {..} => self.do_while_statement(env, node)?,
            AstData::TrailingIfStatement {..} => self.trailing_if(env, node)?,
            AstData::SwitchStatement {..} => self.switch_statement(env, node)?,
            AstData::ExpressionStatement(_) => self.expression_statement(env, node)?,
            AstData::GetProperty {..} => self.get_property(env, node)?,
            AstData::SetProperty {..} => self.set_property(env, node)?,
            AstData::IndexGetter {..} => self.index_getter(env, node)?,
            AstData::IndexSetter {..} => self.index_setter(env, node)?,

            AstData::Program {..} => self.program(env, node)?,
            AstData::StdInclude => self.std_include(env, node)?,
            AstData::Empty => {},

            _ => return Err(self.error(format!("Unknown node: {:#?}", *node))),
        }

        Ok(())
    }

    fn check_type(type_name: &str) -> Option<u8> {
        match type_name {
            "any" => Some(0),
            "none" => Some(1),
            "number" => Some(2),
            "string" => Some(3),
            "bool" => Some(4),
            "list" => Some(5),
            "stringmap" => Some(6),
            "symbol" => Some(7),
            _ => None,
        }
    }
}