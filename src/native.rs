use std::path::Path;

use crate::bytes::rec_vectorise_ty;
use crate::regcode::RegCode;
use crate::{cfg, syntax::*};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::TargetTriple;
use inkwell::types::{BasicMetadataTypeEnum, FloatType, FunctionType, IntType, PointerType};
use inkwell::values::{BasicMetadataValueEnum, FunctionValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

enum LLVMTy<'ctx> {
    Int(IntType<'ctx>),
    Float(FloatType<'ctx>),
    Bool(IntType<'ctx>),
    Str(PointerType<'ctx>),
}

impl<'ctx> LLVMTy<'ctx> {
    fn basic_type(&self) -> BasicMetadataTypeEnum<'ctx> {
        match self {
            LLVMTy::Int(t) => BasicMetadataTypeEnum::IntType(*t),
            LLVMTy::Float(t) => BasicMetadataTypeEnum::FloatType(*t),
            LLVMTy::Bool(t) => BasicMetadataTypeEnum::IntType(*t),
            LLVMTy::Str(t) => BasicMetadataTypeEnum::PointerType(*t),
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
            Type::Double => LLVMTy::Float(self.context.f64_type()),
            _ => LLVMTy::Int(self.context.i64_type()),
        }
    }

    fn create_func_type(&self, params: &[Type], ret: Type) -> FunctionType<'ctx> {
        let ret = self.get_type(&ret);
        let mut param_types = Vec::new();
        for param in params {
            param_types.push(self.get_type(param));
        }
        let params = &param_types
            .iter()
            .map(|t| t.basic_type())
            .collect::<Vec<_>>();
        match ret {
            LLVMTy::Int(t) => {
                let func_type = t.fn_type(params.as_slice(), false);
                func_type
            }
            LLVMTy::Float(t) => {
                let func_type = t.fn_type(params.as_slice(), false);
                func_type
            }
            LLVMTy::Bool(t) => {
                let func_type = t.fn_type(params.as_slice(), false);
                func_type
            }
            LLVMTy::Str(t) => {
                let func_type = t.fn_type(params.as_slice(), false);
                func_type
            }
        }
    }

    pub fn compile_fn(
        &mut self,
        func: &FunctionValue<'ctx>,
        retty: &Type,
        is_main: bool,
        code: &Vec<RegCode>,
    ) {
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

        // allocate all variables in the entry block itself
        // count the number of variables
        let mut max_var_ix = 0;
        for c in code.iter() {
            match c {
                RegCode::Store(var_ix, _, _) => {
                    if *var_ix > max_var_ix {
                        max_var_ix = *var_ix;
                    }
                }
                _ => {}
            }
        }

        for i in 0..=max_var_ix {
            let var = self.builder.build_alloca(i64_type, &format!("var{}", i));
            variables.push(var);
        }

        let cfg = cfg::construct_cfg(&code);

        let mut bbs: Vec<BasicBlock<'ctx>> = Vec::new();
        let range = if is_main { cfg.len() } else { cfg.len() - 1 };
        for i in 0..=range {
            let bb = self
                .context
                .append_basic_block(func.clone(), &format!("bb{}", i));
            bbs.push(bb);
        }

        self.builder.build_unconditional_branch(bbs[0]);

        for (bb_ix, bb) in cfg.iter().enumerate() {
            let code = &bb.code;
            let bb = bbs[bb_ix];
            self.builder.position_at_end(bb);
            for c in code.iter() {
                match c {
                    RegCode::IAdd(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();
                        let v3 = self.builder.build_int_add(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::ISub(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();
                        let v3 = self.builder.build_int_sub(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::IMul(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();
                        let v3 = self.builder.build_int_mul(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::IDiv(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();
                        let v3 = self.builder.build_int_signed_div(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::IRem(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();
                        let v3 = self.builder.build_int_signed_rem(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::INe(r1, r2, r3)
                    | RegCode::IEq(r1, r2, r3)
                    | RegCode::ILt(r1, r2, r3)
                    | RegCode::ILe(r1, r2, r3)
                    | RegCode::IGt(r1, r2, r3)
                    | RegCode::IGe(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();

                        let pred = match c {
                            RegCode::INe(_, _, _) => IntPredicate::NE,
                            RegCode::IEq(_, _, _) => IntPredicate::EQ,
                            RegCode::ILt(_, _, _) => IntPredicate::SLT,
                            RegCode::ILe(_, _, _) => IntPredicate::SLE,
                            RegCode::IGt(_, _, _) => IntPredicate::SGT,
                            RegCode::IGe(_, _, _) => IntPredicate::SGE,
                            _ => unreachable!(),
                        };

                        let v3 = self.builder.build_int_compare(pred, v1, v2, "v3");

                        let v3 = self.builder.build_int_cast(v3, i64_type, "cast");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FAdd(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self
                            .builder
                            .build_load(i64_type, r1, "v1")
                            .into_float_value();
                        let v2 = self
                            .builder
                            .build_load(i64_type, r2, "v2")
                            .into_float_value();
                        let v3 = self.builder.build_float_add(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FSub(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self
                            .builder
                            .build_load(i64_type, r1, "v1")
                            .into_float_value();
                        let v2 = self
                            .builder
                            .build_load(i64_type, r2, "v2")
                            .into_float_value();
                        let v3 = self.builder.build_float_sub(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FMul(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self
                            .builder
                            .build_load(i64_type, r1, "v1")
                            .into_float_value();
                        let v2 = self
                            .builder
                            .build_load(i64_type, r2, "v2")
                            .into_float_value();
                        let v3 = self.builder.build_float_mul(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FDiv(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self
                            .builder
                            .build_load(i64_type, r1, "v1")
                            .into_float_value();
                        let v2 = self
                            .builder
                            .build_load(i64_type, r2, "v2")
                            .into_float_value();
                        let v3 = self.builder.build_float_div(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FRem(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self
                            .builder
                            .build_load(i64_type, r1, "v1")
                            .into_float_value();
                        let v2 = self
                            .builder
                            .build_load(i64_type, r2, "v2")
                            .into_float_value();
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

                        let v1 = self
                            .builder
                            .build_load(i64_type, r1, "v1")
                            .into_float_value();
                        let v2 = self
                            .builder
                            .build_load(i64_type, r2, "v2")
                            .into_float_value();

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
                        let v3 = self.builder.build_int_cast(v3, i64_type, "cast");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::BAnd(r1, r2, r3)
                    | RegCode::BOr(r1, r2, r3)
                    | RegCode::BXor(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();

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

                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();

                        let v2 = self.builder.build_not(v1, "v2");
                        self.builder.build_store(r2, v2);
                    }
                    RegCode::LAnd(r1, r2, r3) | RegCode::LOr(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();

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

                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();

                        let v2 = self.builder.build_not(v1, "v2");
                        self.builder.build_store(r2, v2);
                    }
                    RegCode::BShl(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();

                        let v3 = self.builder.build_left_shift(v1, v2, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::BShr(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();

                        let v3 = self.builder.build_right_shift(v1, v2, false, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FNeg(r1, r2) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];

                        let v1 = self
                            .builder
                            .build_load(i64_type, r1, "v1")
                            .into_float_value();

                        let v2 = self.builder.build_float_neg(v1, "v2");
                        self.builder.build_store(r2, v2);
                    }
                    RegCode::INeg(r1, r2) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];

                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();

                        let v2 = self.builder.build_int_neg(v1, "v2");
                        self.builder.build_store(r2, v2);
                    }
                    RegCode::Make(ty, r, len) => {
                        let mut v = Vec::new();
                        rec_vectorise_ty(ty, &mut v);
                        // call make function
                        let make = self.module.get_function("make").unwrap();
                        let tyy = v[0] as u64;
                        let tyy = self.context.i64_type().const_int(tyy, false);
                        let len =
                            self.builder
                                .build_load(i64_type, registers[*len as usize], "len");
                        let args = &[tyy.into(), len.into()];
                        let v = self
                            .builder
                            .build_call(make, args, "v")
                            .try_as_basic_value()
                            .left()
                            .unwrap();
                        let v = v.into_int_value();
                        self.builder.build_store(registers[*r as usize], v);
                    }
                    RegCode::ListSet(list_reg, idx_reg, val_reg) => {
                        let list_reg = registers[*list_reg as usize];
                        let idx_reg = registers[*idx_reg as usize];
                        let val_reg = registers[*val_reg as usize];
                        let listset = self.module.get_function("listset").unwrap();
                        let list = self.builder.build_load(i64_type, list_reg, "list");
                        let idx = self.builder.build_load(i64_type, idx_reg, "idx");
                        let val = self.builder.build_load(i64_type, val_reg, "val");
                        let val =
                            self.builder
                                .build_int_cast(val.into_int_value(), i64_type, "cast");
                        let args = &[list.into(), idx.into(), val.into()];
                        self.builder.build_call(listset, args, "ret");
                    }
                    RegCode::ListGet(list_reg, idx_reg, val_reg) => {
                        let list_reg = registers[*list_reg as usize];
                        let idx_reg = registers[*idx_reg as usize];
                        let val_reg = registers[*val_reg as usize];
                        let listget = self.module.get_function("listget").unwrap();
                        let list = self.builder.build_load(i64_type, list_reg, "list");
                        let idx = self.builder.build_load(i64_type, idx_reg, "idx");
                        let args = &[list.into(), idx.into()];
                        let v = self
                            .builder
                            .build_call(listget, args, "ret")
                            .try_as_basic_value()
                            .left()
                            .unwrap();
                        self.builder.build_store(val_reg, v.into_int_value());
                    }
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
                        let v = self.builder.build_load(i64_type, r, "v").into_int_value();
                        let v = self.builder.build_int_compare(
                            IntPredicate::EQ,
                            v,
                            self.context.i64_type().const_int(0, false),
                            "v",
                        );
                        self.builder
                            .build_conditional_branch(v, bbs[*ix], bbs[bb_ix + 1]);
                    }
                    RegCode::Call(fn_ix, regs) => {
                        let fn_ptr = self.module.get_function(&self.fn_names[*fn_ix]).unwrap();
                        let mut args = Vec::new();
                        for r in regs {
                            let r = registers[*r as usize];
                            let v = self.builder.build_load(i64_type, r, "v").into();
                            args.push(v);
                        }
                        let res = self.builder.build_call(fn_ptr, args.as_slice(), "ret");
                        self.builder.build_store(
                            registers[0 as usize],
                            res.try_as_basic_value().left().unwrap().into_int_value(),
                        );
                    }
                    RegCode::NCall(fn_ix, regs) => {
                        let fn_name = match fn_ix {
                            0 => "iprint",
                            1 => "fprint",
                            2 => "cprint",
                            3 => "bprint",
                            4 => "sprint",
                            5 => "append",
                            6 => "len",
                            _ => todo!(),
                        };
                        let fn_ptr = self.module.get_function(&fn_name).unwrap();
                        let mut args = Vec::new();
                        for r in regs {
                            let r = registers[*r as usize];
                            let v = self.builder.build_load(i64_type, r, "v");
                            let v_cast: BasicMetadataValueEnum = match fn_ix {
                                1 => self
                                    .builder
                                    .build_float_cast(
                                        v.into_float_value(),
                                        self.context.f64_type(),
                                        "cast",
                                    )
                                    .into(),
                                4 | 6 => self
                                    .builder
                                    .build_int_to_ptr(
                                        v.into_int_value(),
                                        self.context.i8_type().ptr_type(AddressSpace::from(0)),
                                        "cast",
                                    )
                                    .into(),
                                _ => v.into(),
                            };
                            args.push(v_cast);
                        }
                        let res = self.builder.build_call(fn_ptr, args.as_slice(), "ret");
                        self.builder.build_store(
                            registers[0 as usize],
                            res.try_as_basic_value().left().unwrap().into_int_value(),
                        );
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
                                let v = i64_type.const_int(u64::from(byte), false);
                                self.builder.build_store(r, v);
                            }
                            Type::Char => {
                                let byte = const_val.1[0];
                                let v = self.context.i8_type().const_int(u64::from(byte), false);
                                self.builder.build_store(r, v);
                            }
                            Type::Str => {
                                let v = unsafe {
                                    self.builder.build_global_string(
                                        &String::from_utf8(const_val.1).unwrap(),
                                        "str",
                                    )
                                };
                                let v = self.builder.build_pointer_cast(
                                    v.as_pointer_value(),
                                    self.context.i8_type().ptr_type(AddressSpace::from(0)),
                                    "str",
                                );
                                let v_int = self.builder.build_ptr_to_int(
                                    v,
                                    self.context.i64_type(),
                                    "str",
                                );
                                self.builder.build_store(r, v_int);
                            }
                            _ => todo!(),
                        }
                    }
                    RegCode::Load(var_ix, reg_ix, _) => {
                        let varloaded = self.builder.build_load(i64_type, variables[*var_ix], "v");
                        // cast var to i64
                        let varloaded =
                            self.builder
                                .build_int_cast(varloaded.into_int_value(), i64_type, "v");
                        self.builder
                            .build_store(registers[*reg_ix as usize], varloaded);
                    }
                    RegCode::Store(var_ix, reg_ix, _) => {
                        let var = variables[*var_ix];
                        let reg = registers[*reg_ix as usize];
                        let regloaded = self.builder.build_load(i64_type, reg, "v");
                        self.builder.build_store(var, regloaded);
                    }
                    RegCode::Cast(_, _, _, _) => todo!(),
                    RegCode::Move(r1, r2) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let v1 = self.builder.build_load(i64_type, r2, "v1");
                        self.builder.build_store(r1, v1);
                    }
                    RegCode::Return(reg) => {
                        let reg = registers[*reg as usize];
                        let v = self.builder.build_load(i64_type, reg, "v");
                        self.builder.build_return(Some(&v));
                    }
                    RegCode::Push(_) => {}
                    RegCode::Spawn => todo!(),
                    RegCode::Nop => {}
                    RegCode::FreeRegs => {}
                    RegCode::Pop(_) => {}
                    RegCode::StackMap(_) => {}
                    RegCode::Super(_) => {}
                    RegCode::CoroCall(_, _) => todo!(),
                    RegCode::Ref(_, _) => todo!(),
                    RegCode::Deref(_, _) => todo!(),
                    RegCode::DerefAssign(_, _, _) => todo!(),
                }
            }
            match code[code.len() - 1] {
                RegCode::Jmp(_) => {}
                RegCode::JmpIfNot(_1, _) => {}
                _ => {
                    let lim = if is_main { code.len() } else { code.len() - 1 };
                    if bb_ix < lim {
                        self.builder.build_unconditional_branch(bbs[bb_ix + 1]);
                    }
                }
            }
        }

        self.builder.position_at_end(bbs[bbs.len() - 1]);

        if is_main {
            // call exit
            let exit_fn = self.module.get_function("exit").unwrap();
            self.builder
                .build_call(exit_fn, &[i64_type.const_int(0, false).into()], "ret");
        }

        if *retty == Type::Void {
            self.builder
                .build_return(Some(&i64_type.const_int(0, false)));
        }
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
        self.module.add_function(
            "exit",
            self.create_func_type(&[Type::Int], Type::Void),
            None,
        );
        self.module.add_function(
            "append",
            self.create_func_type(&[Type::Int, Type::Int], Type::Void), // list, val
            None,
        );
        self.module.add_function(
            "len",
            self.create_func_type(&[Type::Int], Type::Int), // list
            None,
        );
        self.module.add_function(
            "make",
            self.create_func_type(&[Type::Int, Type::Int], Type::Int), // type, len
            None,
        );
        self.module.add_function(
            "listset",
            self.create_func_type(&[Type::Int, Type::Int, Type::Int], Type::Void), // list, idx, val
            None,
        );
        self.module.add_function(
            "listget",
            self.create_func_type(&[Type::Int, Type::Int], Type::Int), // list, idx
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
            self.compile_fn(&funcs[i], &func.0.retty, func.0.name == "main", func.1);
        }

        // mem2reg - no passes working
        // let pass_manager = PassManager::create(());
        // pass_manager.add_instruction_simplify_pass();
        // pass_manager.run_on(&self.module);

        let triple = TargetTriple::create("x86_64-unknown-linux-gnu");
        self.module.set_triple(&triple);
        self.module.print_to_stderr();
        self.module.write_bitcode_to_path(Path::new("./exe.bc"));
    }
}
