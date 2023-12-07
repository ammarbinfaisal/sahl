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
use inkwell::values::{
    BasicMetadataValueEnum, FloatValue, FunctionValue, InstructionOpcode, IntValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

enum LLVMTy<'ctx> {
    Int(IntType<'ctx>),
    Float(FloatType<'ctx>),
    Bool(IntType<'ctx>),
    Str(PointerType<'ctx>),
    Void,
}

impl<'ctx> LLVMTy<'ctx> {
    fn basic_type(&self) -> BasicMetadataTypeEnum<'ctx> {
        match self {
            LLVMTy::Int(t) => BasicMetadataTypeEnum::IntType(*t),
            LLVMTy::Float(t) => BasicMetadataTypeEnum::FloatType(*t),
            LLVMTy::Bool(t) => BasicMetadataTypeEnum::IntType(*t),
            LLVMTy::Str(t) => BasicMetadataTypeEnum::PointerType(*t),
            LLVMTy::Void => unreachable!("This path should not be reached"),
        }
    }
}

pub struct Compiler<'ctx, 'src> {
    pub context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    consts: Vec<(Type<'src>, Vec<u8>)>,
    fn_names: Vec<&'src str>,
}

impl<'ctx, 'src> Compiler<'ctx, 'src> {
    pub fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        builder: Builder<'ctx>,
        consts: Vec<(Type<'src>, Vec<u8>)>,
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
            Type::Custom(_) => LLVMTy::Int(self.context.i64_type()),
            Type::List(_) => LLVMTy::Int(self.context.i64_type()),
            Type::Tuple(_) => LLVMTy::Int(self.context.i64_type()),
            Type::Map(_, _) => LLVMTy::Int(self.context.i64_type()),
            Type::Chan(_) => LLVMTy::Int(self.context.i64_type()),
            _ => LLVMTy::Void,
        }
    }

    fn int_to_float_reinterpret(&self, v1: IntValue<'ctx>) -> FloatValue<'ctx> {
        self.builder
            .build_bitcast(v1, self.context.f64_type(), "cast")
            .into_float_value()
    }

    fn float_to_int_reinterpret(&self, v1: FloatValue<'ctx>) -> IntValue<'ctx> {
        self.builder
            .build_bitcast(v1, self.context.i64_type(), "cast")
            .into_int_value()
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
            LLVMTy::Void => {
                let func_type = self.context.void_type().fn_type(params.as_slice(), false);
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
                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();
                        let v1_f = self.int_to_float_reinterpret(v1);
                        let v2_f = self.int_to_float_reinterpret(v2);
                        let v3 = self.builder.build_float_add(v1_f, v2_f, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FSub(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();
                        let v1_f = self.int_to_float_reinterpret(v1);
                        let v2_f = self.int_to_float_reinterpret(v2);
                        let v3 = self.builder.build_float_sub(v1_f, v2_f, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FMul(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];
                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();
                        let v1_f = self.int_to_float_reinterpret(v1);
                        let v2_f = self.int_to_float_reinterpret(v2);
                        let v3 = self.builder.build_float_mul(v1_f, v2_f, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FDiv(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();
                        let v1_f = self.int_to_float_reinterpret(v1);
                        let v2_f = self.int_to_float_reinterpret(v2);
                        let v3 = self.builder.build_float_div(v1_f, v2_f, "v3");
                        self.builder.build_store(r3, v3);
                    }
                    RegCode::FRem(r1, r2, r3) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let r3 = registers[*r3 as usize];

                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();
                        let v1_f = self.int_to_float_reinterpret(v1);
                        let v2_f = self.int_to_float_reinterpret(v2);
                        let v3 = self.builder.build_float_rem(v1_f, v2_f, "v3");
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

                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v2 = self.builder.build_load(i64_type, r2, "v2").into_int_value();
                        let v1_f = self.int_to_float_reinterpret(v1);
                        let v2_f = self.int_to_float_reinterpret(v2);

                        let pred = match c {
                            RegCode::FNe(_, _, _) => FloatPredicate::ONE,
                            RegCode::FEq(_, _, _) => FloatPredicate::OEQ,
                            RegCode::FLt(_, _, _) => FloatPredicate::OLT,
                            RegCode::FLe(_, _, _) => FloatPredicate::OLE,
                            RegCode::FGt(_, _, _) => FloatPredicate::OGT,
                            RegCode::FGe(_, _, _) => FloatPredicate::OGE,
                            _ => unreachable!(),
                        };

                        let v3 = self.builder.build_float_compare(pred, v1_f, v2_f, "v3");
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

                        let v1 = self.builder.build_load(i64_type, r1, "v1").into_int_value();
                        let v1 = self.int_to_float_reinterpret(v1);

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
                    RegCode::TupleSet(ex, idx, val) => {
                        let ex = registers[*ex as usize];
                        let idx = registers[*idx as usize];
                        let val = registers[*val as usize];
                        let tupleset = self.module.get_function("listset").unwrap();
                        let ex = self.builder.build_load(i64_type, ex, "ex");
                        let idx = self.builder.build_load(i64_type, idx, "idx");
                        let val = self.builder.build_load(i64_type, val, "val");
                        let val =
                            self.builder
                                .build_int_cast(val.into_int_value(), i64_type, "cast");
                        let args = &[ex.into(), idx.into(), val.into()];
                        self.builder.build_call(tupleset, args, "ret");
                    }
                    RegCode::TupleGet(ex, idx, res) => {
                        let ex = registers[*ex as usize];
                        let idx = registers[*idx as usize];
                        let res = registers[*res as usize];
                        let tupleget = self.module.get_function("listget").unwrap();
                        let ex = self.builder.build_load(i64_type, ex, "ex");
                        let idx = self.builder.build_load(i64_type, idx, "idx");
                        let args = &[ex.into(), idx.into()];
                        let v = self
                            .builder
                            .build_call(tupleget, args, "ret")
                            .try_as_basic_value()
                            .left()
                            .unwrap();
                        self.builder.build_store(res, v.into_int_value());
                    }
                    RegCode::Tuple(len, res, _tys) => {
                        let res = registers[*res as usize];
                        let tuple = self.module.get_function("make_list").unwrap();
                        let len = i64_type.const_int(*len as u64, false);
                        let args = &[len.into()];
                        let v = self
                            .builder
                            .build_call(tuple, args, "ret")
                            .try_as_basic_value()
                            .left()
                            .unwrap();
                        self.builder.build_store(res, v.into_int_value());
                    }
                    RegCode::StrGet(_, _, _) => todo!(),
                    RegCode::MapGet(_, _, _) => todo!(),
                    RegCode::MapSet(_, _, _) => todo!(),
                    RegCode::ChanSend(chan_var, var_reg) => {
                        let chan_var = variables[*chan_var as usize];
                        let var_reg = registers[*var_reg as usize];
                        let chansend = self.module.get_function("chansend").unwrap();
                        let chan = self.builder.build_load(i64_type, chan_var, "chan");
                        let val = self.builder.build_load(i64_type, var_reg, "val");
                        let args = &[chan.into(), val.into()];
                        self.builder.build_call(chansend, args, "ret");
                    }
                    RegCode::ChanRecv(chan_var, var_reg) => {
                        let chan_var = variables[*chan_var as usize];
                        let var_reg = registers[*var_reg as usize];
                        let chanrecv = self.module.get_function("chanrecv").unwrap();
                        let chan = self.builder.build_load(i64_type, chan_var, "chan");
                        let args = &[chan.into()];
                        let v = self
                            .builder
                            .build_call(chanrecv, args, "ret")
                            .try_as_basic_value()
                            .left()
                            .unwrap();
                        self.builder.build_store(var_reg, v.into_int_value());
                    }
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
                        let res = res.try_as_basic_value().left();
                        match res {
                            Some(res) => match res.into() {
                                BasicMetadataValueEnum::FloatValue(v) => {
                                    let reinp = self.float_to_int_reinterpret(v);
                                    self.builder.build_store(registers[0 as usize], reinp);
                                }
                                _ => {
                                    self.builder.build_store(registers[0 as usize], res);
                                }
                            },
                            None => {}
                        }
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
                            7 => "pop",
                            8 => "make_variant",
                            9 => "is_variant",
                            10 => "get_variant",
                            _ => todo!(),
                        };
                        let fn_ptr = self.module.get_function(&fn_name).unwrap();
                        let mut args = Vec::new();
                        for r in regs {
                            let r = registers[*r as usize];
                            let v = self.builder.build_load(i64_type, r, "v");
                            let v_cast: BasicMetadataValueEnum = match fn_ix {
                                1 => self.int_to_float_reinterpret(v.into_int_value()).into(),
                                2 | 3 => self
                                    .builder
                                    .build_int_cast(v.into_int_value(), i64_type, "cast")
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
                        let res = res.try_as_basic_value().left();
                        if let Some(res) = res {
                            let res = res.into_int_value();
                            self.builder.build_store(registers[0 as usize], res);
                        }
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
                                let len = const_val.1.len();
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
                                let str_obj = self.builder.build_call(
                                    self.module.get_function("make_string").unwrap(),
                                    &[
                                        v.into(),
                                        i64_type.const_int(len.try_into().unwrap(), false).into(),
                                    ],
                                    "str_obj",
                                );
                                let str_obj = str_obj.try_as_basic_value().left().unwrap();
                                // cast str_obj to i64
                                let str_obj = self.builder.build_int_cast(
                                    str_obj.into_int_value(),
                                    i64_type,
                                    "str_obj",
                                );
                                self.builder.build_store(r, str_obj);
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
                    RegCode::Cast(reg, t1, t2, res) => {
                        let reg = registers[*reg as usize];
                        let v = self.builder.build_load(i64_type, reg, "v");
                        match (t1, t2) {
                            (Type::Int, Type::Double) => {
                                // cast (i64) v to (f64) v
                                let resv = self.builder.build_cast(
                                    InstructionOpcode::SIToFP,
                                    v,
                                    self.context.f64_type(),
                                    "cast",
                                );
                                self.builder.build_store(registers[*res as usize], resv);
                            }
                            (Type::Double, Type::Int) => {
                                // cast (f64) v to (i64) v
                                let resv = self.builder.build_cast(
                                    InstructionOpcode::FPToSI,
                                    v,
                                    self.context.i64_type(),
                                    "cast",
                                );
                                self.builder.build_store(registers[*res as usize], resv);
                            }
                            (Type::Bool, Type::Int) => {
                                let resv = self.builder.build_int_cast(
                                    v.into_int_value(),
                                    i64_type,
                                    "cast",
                                );
                                self.builder.build_store(registers[*res as usize], resv);
                            }
                            (Type::Int, Type::Char) => {
                                // basically do x & 0xFF
                                let resv = self.builder.build_and(
                                    v.into_int_value(),
                                    i64_type.const_int(0xFF, false),
                                    "cast",
                                );
                                self.builder.build_store(registers[*res as usize], resv);
                            }
                            (Type::Char, Type::Int) => {
                                // basically do x & 0xFF
                                let resv = self.builder.build_and(
                                    v.into_int_value(),
                                    i64_type.const_int(0xFF, false),
                                    "cast",
                                );
                                self.builder.build_store(registers[*res as usize], resv);
                            }
                            _ => todo!(),
                        };
                    }
                    RegCode::Move(r1, r2) => {
                        let r1 = registers[*r1 as usize];
                        let r2 = registers[*r2 as usize];
                        let v1 = self.builder.build_load(i64_type, r2, "v1");
                        self.builder.build_store(r1, v1);
                    }
                    RegCode::Return(reg) => {
                        if *retty == Type::Void {
                            self.builder.build_return(None);
                            continue;
                        }
                        let reg = registers[*reg as usize];
                        let v = self.builder.build_load(i64_type, reg, "v").into_int_value();
                        match *retty {
                            Type::Double => {
                                let v = self.int_to_float_reinterpret(v);
                                self.builder.build_return(Some(&v));
                            }
                            _ => {
                                self.builder.build_return(Some(&v));
                            }
                        }
                    }
                    RegCode::VoidReturn => {
                        self.builder.build_return(None);
                    }
                    RegCode::Push(_) => {}
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
                    if bb_ix < range {
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
            self.builder.build_return(None);
        }
    }

    pub fn compile_program(&mut self, program: &Program<'src>, code: Vec<Vec<RegCode>>) {
        let functions = [
            ("iprint", vec![Type::Int], Type::Void),
            ("fprint", vec![Type::Double], Type::Void),
            ("cprint", vec![Type::Char], Type::Void),
            ("bprint", vec![Type::Bool], Type::Void),
            ("sprint", vec![Type::Str], Type::Void),
            ("exit", vec![Type::Int], Type::Void),
            ("append", vec![Type::Int, Type::Int], Type::Void),
            ("pop", vec![Type::Int], Type::Int),
            ("make_list", vec![Type::Int], Type::Int),
            ("list", vec![Type::Int, Type::Int, Type::Int], Type::Void),
            ("len", vec![Type::Int], Type::Int),
            ("make", vec![Type::Int, Type::Int], Type::Int),
            ("make_string", vec![Type::Int], Type::Int),
            ("listset", vec![Type::Int, Type::Int, Type::Int], Type::Void),
            ("listget", vec![Type::Int, Type::Int], Type::Int),
            ("chansend", vec![Type::Int, Type::Int], Type::Void),
            ("chanrecv", vec![Type::Int], Type::Int),
            ("make_variant", vec![Type::Int, Type::Int], Type::Int),
            ("is_variant", vec![Type::Int, Type::Int], Type::Int),
            ("get_variant", vec![Type::Int], Type::Int),
        ];

        for (name, params, retty) in functions.iter() {
            let func_type = self.create_func_type(params.as_slice(), retty.clone());
            self.module.add_function(name, func_type, None);
        }

        let fns = &program
            .top_levels
            .iter()
            .filter(|tl| match tl {
                TopLevel::Func(_) => true,
                _ => false,
            })
            .map(|tl| match tl {
                TopLevel::Func(f) => f,
                _ => unreachable!(),
            })
            .collect::<Vec<_>>();

        let mut funcs = Vec::new();
        for func in fns.iter() {
            let params = func
                .args
                .iter()
                .map(|arg| arg.ty.clone())
                .collect::<Vec<_>>();
            self.fn_names.push(func.name);
            let func_type = self.create_func_type(params.as_slice(), func.retty.clone());
            let func = if func.name == "main" {
                self.module.add_function("sahl_main", func_type, None)
            } else {
                self.module.add_function(&func.name, func_type, None)
            };
            funcs.push(func);
        }

        for (i, func) in fns.iter().zip(code.iter()).enumerate() {
            if func.0.externed {
                continue;
            }
            self.compile_fn(&funcs[i], &func.0.retty, func.0.name == "main", func.1);
        }

        // mem2reg - no passes working
        // let pass_manager = PassManager::create(());
        // pass_manager.add_promote_memory_to_register_pass();
        // pass_manager.run_on(&self.module);

        let triple = TargetTriple::create("x86_64-unknown-linux-gnu");
        self.module.set_triple(&triple);
        self.module.print_to_stderr();
        self.module.write_bitcode_to_path(Path::new("./exe.bc"));
    }
}
