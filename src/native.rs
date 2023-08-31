use std::path::Path;

use crate::regcode::RegCode;
use crate::{cfg, syntax::*};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::{BasicMetadataTypeEnum, FloatType, FunctionType, IntType, PointerType};
use inkwell::values::FunctionValue;
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

enum LLVMTy<'ctx> {
    Int(IntType<'ctx>),
    Float(FloatType<'ctx>),
    Bool(IntType<'ctx>),
    Str(PointerType<'ctx>),
    List(PointerType<'ctx>),
}

impl<'ctx> LLVMTy<'ctx> {
    fn into_pointer_type(self) -> PointerType<'ctx> {
        match self {
            LLVMTy::Int(t) => t.ptr_type(AddressSpace::from(0)),
            LLVMTy::Float(t) => t.ptr_type(AddressSpace::from(0)),
            LLVMTy::Bool(t) => t.ptr_type(AddressSpace::from(0)),
            LLVMTy::Str(t) => t,
            LLVMTy::List(t) => t,
        }
    }

    fn basic_type(&self) -> BasicMetadataTypeEnum<'ctx> {
        match self {
            LLVMTy::Int(t) => BasicMetadataTypeEnum::IntType(*t),
            LLVMTy::Float(t) => BasicMetadataTypeEnum::FloatType(*t),
            LLVMTy::Bool(t) => BasicMetadataTypeEnum::IntType(*t),
            LLVMTy::Str(t) => BasicMetadataTypeEnum::PointerType(*t),
            LLVMTy::List(t) => BasicMetadataTypeEnum::PointerType(*t),
        }
    }
}

pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    consts: Vec<(Type, Vec<u8>)>,
    fn_names: Vec<String>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        builder: Builder<'ctx>,
        consts: Vec<(Type, Vec<u8>)>,
    ) -> Self {
        Self {
            context,
            module,
            builder,
            consts,
            fn_names: Vec::new(),
        }
    }

    fn get_type(&self, t: &Type) -> LLVMTy<'ctx> {
        match t {
            Type::Int => LLVMTy::Int(self.context.i64_type()),
            Type::Bool => LLVMTy::Bool(self.context.bool_type()),
            Type::Char => LLVMTy::Int(self.context.i8_type()),
            Type::Str => LLVMTy::Str(self.context.i8_type().ptr_type(AddressSpace::from(0))),
            Type::List(t) => LLVMTy::List(self.get_type(t).into_pointer_type()),
            Type::Double => LLVMTy::Float(self.context.f64_type()),
            _ => todo!(),
        }
    }

    fn create_func_type(&self, params: &[Type], ret: Type) -> FunctionType<'ctx> {
        let mut param_types = Vec::new();
        for param in params {
            param_types.push(self.get_type(param));
        }
        let params = &param_types
            .iter()
            .map(|t| t.basic_type())
            .collect::<Vec<_>>();
        if ret == Type::Void {
            let void_type = self.context.void_type();
            let func_type = void_type.fn_type(params.as_slice(), false);
            return func_type;
        }
        let ret_type = self.get_type(&ret);
        let func_type = ret_type
            .into_pointer_type()
            .fn_type(params.as_slice(), false);
        func_type
    }

    pub fn compile_fn(&mut self, func: &FunctionValue<'ctx>, code: Vec<RegCode>) {
        let i64_type = self.context.i64_type();
        let entry = self.context.append_basic_block(func.clone(), "entry");
        self.builder.position_at_end(entry);

        let mut variables = Vec::new();

        // move args to v{0..n}
        for (i, arg) in func.get_param_iter().enumerate() {
            let arg1 = self.builder.build_alloca(i64_type, &format!("var{}", i));
            self.builder.build_store(arg1, arg);
            variables.push(arg1);
        }

        let mut registers = Vec::new();
        for r in 0..256 {
            registers.push(self.builder.build_alloca(i64_type, &format!("r{}", r)));
        }

        let cfg = cfg::construct_cfg(&code);

        let mut bbs: Vec<BasicBlock<'ctx>> = Vec::new();
        for i in 0..=cfg.len() {
            let bb = self
                .context
                .append_basic_block(func.clone(), &format!("bb{}", i));
            bbs.push(bb);
        }

        self.builder.build_unconditional_branch(bbs[0]);

        for (i, bb) in cfg.iter().enumerate() {
            println!("{:?}", bb);
            let code = &bb.code;
            let bb = bbs[i];
            self.builder.position_at_end(bb);
            for c in code.iter() {
                match c {
                    RegCode::IAdd(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();
                        let v3 = self.builder.build_int_add(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::ISub(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();
                        let v3 = self.builder.build_int_sub(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::IMul(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();
                        let v3 = self.builder.build_int_mul(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::IDiv(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();
                        let v3 = self.builder.build_int_signed_div(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::IRem(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();
                        let v3 = self.builder.build_int_signed_rem(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::INe(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();
                        let v3 = self
                            .builder
                            .build_int_compare(IntPredicate::NE, v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::IEq(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();
                        let v3 = self
                            .builder
                            .build_int_compare(IntPredicate::EQ, v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::ILt(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();
                        let v3 = self
                            .builder
                            .build_int_compare(IntPredicate::SLT, v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::ILe(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();
                        let v3 = self
                            .builder
                            .build_int_compare(IntPredicate::SLE, v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::IGt(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();
                        let v3 = self
                            .builder
                            .build_int_compare(IntPredicate::SGT, v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::IGe(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();
                        let v3 = self
                            .builder
                            .build_int_compare(IntPredicate::SGE, v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FAdd(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_float_value();
                        let v2 = self.builder.build_load(r2, "v2").into_float_value();
                        let v3 = self.builder.build_float_add(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FSub(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_float_value();
                        let v2 = self.builder.build_load(r2, "v2").into_float_value();
                        let v3 = self.builder.build_float_sub(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FMul(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(r1, "v1").into_float_value();
                        let v2 = self.builder.build_load(r2, "v2").into_float_value();
                        let v3 = self.builder.build_float_mul(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FDiv(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self.builder.build_load(r1, "v1").into_float_value();
                        let v2 = self.builder.build_load(r2, "v2").into_float_value();
                        let v3 = self.builder.build_float_div(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FRem(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self.builder.build_load(r1, "v1").into_float_value();
                        let v2 = self.builder.build_load(r2, "v2").into_float_value();
                        let v3 = self.builder.build_float_rem(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FNe(r1, r2, r3)
                    | RegCode::FEq(r1, r2, r3)
                    | RegCode::FLt(r1, r2, r3)
                    | RegCode::FLe(r1, r2, r3)
                    | RegCode::FGt(r1, r2, r3)
                    | RegCode::FGe(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self.builder.build_load(r1, "v1").into_float_value();
                        let v2 = self.builder.build_load(r2, "v2").into_float_value();

                        let pred = match c {
                            RegCode::FNe(_, _, _) => FloatPredicate::ONE,
                            RegCode::FEq(_, _, _) => FloatPredicate::OEQ,
                            RegCode::FLt(_, _, _) => FloatPredicate::OLT,
                            RegCode::FLe(_, _, _) => FloatPredicate::OLE,
                            RegCode::FGt(_, _, _) => FloatPredicate::OGT,
                            RegCode::FGe(_, _, _) => FloatPredicate::OGE,
                            _ => unreachable!(),
                        };

                        let v3 = self.builder.build_float_compare(pred, v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::BAnd(r1, r2, r3)
                    | RegCode::BOr(r1, r2, r3)
                    | RegCode::BXor(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();

                        let build_fn = match c {
                            RegCode::BAnd(_, _, _) => Builder::build_and,
                            RegCode::BOr(_, _, _) => Builder::build_or,
                            RegCode::BXor(_, _, _) => Builder::build_xor,
                            _ => unreachable!(),
                        };

                        let v3 = build_fn(&self.builder, v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::BNot(r1, r2) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];

                        let v1 = self.builder.build_load(r1, "v1").into_int_value();

                        let v2 = self.builder.build_not(v1, "v2");
                        self.builder.build_store(r2, v2);
                    }
                    RegCode::LAnd(r1, r2, r3) | RegCode::LOr(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();

                        let build_fn = match c {
                            RegCode::LAnd(_, _, _) => Builder::build_and,
                            RegCode::LOr(_, _, _) => Builder::build_or,
                            _ => unreachable!(),
                        };

                        let v3 = build_fn(&self.builder, v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::LNot(r1, r2) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];

                        let v1 = self.builder.build_load(r1, "v1").into_int_value();

                        let v2 = self.builder.build_not(v1, "v2");
                        self.builder.build_store(r2, v2);
                    }
                    RegCode::BShl(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();

                        let v3 = self.builder.build_left_shift(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::BShr(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self.builder.build_load(r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(r2, "v2").into_int_value();

                        let v3 = self.builder.build_right_shift(v1, v2, false, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FNeg(r1, r2) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];

                        let v1 = self.builder.build_load(r1, "v1").into_float_value();

                        let v2 = self.builder.build_float_neg(v1, "v2");
                        self.builder.build_store(r2, v2);
                    }
                    RegCode::INeg(r1, r2) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];

                        let v1 = self.builder.build_load(r1, "v1").into_int_value();

                        let v2 = self.builder.build_int_neg(v1, "v2");
                        self.builder.build_store(r2, v2);
                    }
                    RegCode::Make(_, _, _) => todo!(),
                    RegCode::ListSet(_, _, _) => todo!(),
                    RegCode::ListGet(_, _, _) => todo!(),
                    RegCode::List(_, _, _) => todo!(),
                    RegCode::TupleGet(_, _, _) => todo!(),
                    RegCode::Tuple(_, _, _) => todo!(),
                    RegCode::StrGet(_, _, _) => todo!(),
                    RegCode::MapGet(_, _, _) => todo!(),
                    RegCode::MapSet(_, _, _) => todo!(),
                    RegCode::ChanSend(_, _) => todo!(),
                    RegCode::ChanRecv(_, _) => todo!(),
                    RegCode::Jmp(ix) => {
                        self.builder.build_unconditional_branch(bbs[*ix]);
                    }
                    RegCode::JmpIfNot(r, ix) => {
                        let r = registers[*r as usize];
                        let v = self.builder.build_load(r, "v").into_int_value();
                        let v = self.builder.build_int_compare(
                            IntPredicate::EQ,
                            v,
                            self.context.bool_type().const_int(0, false),
                            "v",
                        );
                        self.builder
                            .build_conditional_branch(v, bbs[*ix], bbs[i + 1]);
                    }
                    RegCode::Call(fn_ix, regs) => {
                        let fn_ptr = self.module.get_function(&self.fn_names[*fn_ix]).unwrap();
                        let mut args = Vec::new();
                        for r in regs {
                            let r = registers[*r as usize];
                            let v = self.builder.build_load(r, "v").into();
                            args.push(v);
                        }
                        self.builder.build_call(fn_ptr, args.as_slice(), "ret");
                    }
                    RegCode::NCall(fn_ix, regs) => {
                        let fn_name = match fn_ix {
                            0 => "iprint",
                            1 => "fprint",
                            2 => "cprint",
                            3 => "bprint",
                            4 => "sprint",
                            _ => todo!(),
                        };
                        let fn_ptr = self.module.get_function(&fn_name).unwrap();
                        let mut args = Vec::new();
                        for r in regs {
                            let r = registers[*r as usize];
                            let v = self.builder.build_load(r, "v").into();
                            args.push(v);
                        }
                        self.builder.build_call(fn_ptr, args.as_slice(), "ret");
                    }
                    RegCode::Const(const_ix, r) => {
                        let const_val = self.consts[*const_ix as usize].clone();
                        let r = registers[*r as usize];
                        match const_val.0 {
                            Type::Int => {
                                // const_val.1 is just i64 value in bytes
                                let mut bytes = [0, 0, 0, 0, 0, 0, 0, 0];
                                for i in 0..8 {
                                    bytes[i] = const_val.1[i];
                                }
                                let v = self
                                    .context
                                    .i64_type()
                                    .const_int(u64::from_le_bytes(bytes), true);
                                self.builder.build_store(r, v);
                            }
                            Type::Double => {
                                let mut bytes = [0, 0, 0, 0, 0, 0, 0, 0];
                                for i in 0..8 {
                                    bytes[i] = const_val.1[i];
                                }
                                let v = self
                                    .context
                                    .f64_type()
                                    .const_float(f64::from_le_bytes(bytes));
                                self.builder.build_store(r, v);
                            }
                            Type::Bool => {
                                let byte = const_val.1[0];
                                let v = self.context.bool_type().const_int(u64::from(byte), false);
                                self.builder.build_store(r, v);
                            }
                            Type::Char => {
                                let byte = const_val.1[0];
                                let v = self.context.i8_type().const_int(u64::from(byte), false);
                                self.builder.build_store(r, v);
                            }
                            Type::Str => {
                                let v = self.builder.build_global_string_ptr(
                                    std::str::from_utf8(const_val.1.as_slice()).unwrap(),
                                    "str",
                                );
                                self.builder.build_store(r, v);
                            }
                            _ => todo!(),
                        }
                    }
                    RegCode::Load(var_ix, reg_ix, _) => {
                        let varloaded = self.builder.build_load(variables[*var_ix], "v");
                        // cast var to i64
                        let varloaded = self.builder.build_int_cast(
                            varloaded.into_int_value(),
                            i64_type,
                            "v",
                        );
                        self.builder
                            .build_store(registers[*reg_ix as usize], varloaded);
                    }
                    RegCode::Store(var_ix, reg_ix, _) => {
                        let var = if *var_ix == variables.len() {
                            let v = self.builder.build_alloca(i64_type, "var");
                            variables.push(v);
                            v
                        } else {
                            variables[*var_ix]
                        };
                        let reg = registers[*reg_ix as usize];
                        let regloaded = self.builder.build_load(reg, "v");
                        self.builder.build_store(var, regloaded);
                    }
                    RegCode::Cast(_, _, _, _) => todo!(),
                    RegCode::Move(r1, r2) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let v1 = self.builder.build_load(r1, "v1");
                        self.builder.build_store(r2, v1);
                    }
                    RegCode::Return(_) => {}
                    RegCode::Push(_) => todo!(),
                    RegCode::Spawn => todo!(),
                    RegCode::Nop => {}
                    RegCode::FreeRegs => {}
                    RegCode::Pop(_) => todo!(),
                    RegCode::StackMap(_) => todo!(),
                    RegCode::PrintLock => todo!(),
                    RegCode::PrintUnlock => todo!(),
                    RegCode::Super(_) => {}
                    RegCode::CoroCall(_, _) => todo!(),
                }
            }
            match code[code.len() - 1] {
                RegCode::Jmp(_) => {}
                RegCode::JmpIfNot(_1, _) => {}
                _ => {
                    self.builder.build_unconditional_branch(bbs[i + 1]);
                }
            }
        }
        self.builder.position_at_end(bbs[bbs.len() - 1]);
        self.builder.build_return(None);
    }

    pub fn compile_program(&mut self, program: &Program, code: Vec<Vec<RegCode>>) {
        self.module.add_function(
            "iprint",
            self.create_func_type(&[Type::Int], Type::Void),
            None,
        );
        self.module.add_function(
            "fprint",
            self.create_func_type(&[Type::Double], Type::Void),
            None,
        );
        self.module.add_function(
            "cprint",
            self.create_func_type(&[Type::Char], Type::Void),
            None,
        );
        self.module.add_function(
            "bprint",
            self.create_func_type(&[Type::Bool], Type::Void),
            None,
        );
        self.module.add_function(
            "sprint",
            self.create_func_type(&[Type::Str], Type::Void),
            None,
        );

        let mut funcs = Vec::new();
        for func in program.funcs.iter() {
            let params = func
                .args
                .iter()
                .map(|arg| arg.ty.clone())
                .collect::<Vec<_>>();
            self.fn_names.push(func.name.clone());
            let func_type = self.create_func_type(params.as_slice(), func.retty.clone());
            let func = self.module.add_function(&func.name, func_type, None);
            funcs.push(func);
        }

        for (i, func) in program.funcs.iter().zip(code.iter()).enumerate() {
            self.compile_fn(&funcs[i], func.1.clone());
        }

        // mem2reg
        let pass_manager = PassManager::create(());
        pass_manager.add_promote_memory_to_register_pass();
        pass_manager.run_on(&self.module);

        self.module.print_to_stderr();
        self.module.write_bitcode_to_path(Path::new("./exe.bc"));
    }
}
