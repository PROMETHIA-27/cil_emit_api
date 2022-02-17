use anyhow::Result;
use netcorehost::{hostfxr::*, pdcstring::*, *};
use std::sync::Arc;

mod enums;
mod newtypes;

pub use enums::*;
pub use newtypes::*;

/// This macro helps me not have to do as much text processing manually to convert all these functions to rust fn ptrs.
/// Just copy + paste the signature and go.
macro_rules! csharp_sig_to_fn_ptr {
    (
        pub struct $struct_name:ident {
            $($ret_ty:ident $cs_name:ident($($param_ty:ident $param_name:ident),*)),* $(,)?
        }
        $ds:tt
    ) => {
        #[::to_snake_case::convert_fields_to_snake_case]
        pub struct $struct_name {
            $(
                $cs_name : extern "system" fn($(convert_cs_ty_to_rs_ty!($param_ty)),*) -> convert_cs_ty_to_rs_ty!($ret_ty)
            ),*
        }

        fn collect_functions(loader: AssemblyDelegateLoader<PdCString>) -> Result<Arc<ClrAsmCtx>> {
            macro_rules! get_emit_func {
                ($ds name:ident) => {{
                    let func = loader.get_function_pointer_for_unmanaged_callers_only_method(
                        pdcstr!("CILEmitAPI.EmitAPI, CILEmitAPI, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"), 
                        &PdCString::from_os_str(stringify!($ds name)).unwrap(),
                    )?;
    
                    // Safety: This could potentially cause UB but this should always be a ptr -> fnptr cast, 
                    // which should practically be sound in every case unless I'm missing something
                    let func = unsafe { std::mem::transmute::<MethodWithUnknownSignature, _>(func) };

                    println!("Loaded {}", stringify!($ds name));
    
                    func
                }}
            }
    
            macro_rules! get_emit_funcs {
                (
                    $ds ($ds name:ident)*
                ) => {
                    $ds (
                        let to_snake_case::to_snake_case!($ds name) = get_emit_func!($ds name);
                    )*
                }
            }
    
            get_emit_funcs![
                $($cs_name)*
            ];
    
            Ok(Arc::new(to_snake_case::in_group_to_snake_case!(ClrAsmCtx {
                $($cs_name),*
            })))
        }
    };
}

macro_rules! convert_cs_ty_to_rs_ty {
    (IntPtr) => { isize };
    (int) => { i32 };
    (Int64) => { i64 };
    (uint) => { u32 };
    (ushort) => { u16 };
    (void) => { () };
}

csharp_sig_to_fn_ptr!{
    pub struct ClrAsmCtx {
        void FreeHandle(IntPtr handle),
    
        // AssemblyDef
        IntPtr AssemblyDefCtor(IntPtr name, int name_length, int vers_major, int vers_minor, int vers_build, int vers_revision),
        IntPtr GetAssemblyModules(IntPtr assembly),
    
        // ModuleDef
        IntPtr ModuleDefCtor(IntPtr name_ptr, int name_length),
        void SetModuleKind(IntPtr module, int kind),
        void SetModuleEntryPoint(IntPtr module, IntPtr method),
        IntPtr GetGlobalType(IntPtr module),
        IntPtr GetCoreLibType(IntPtr module, int core_lib_type),
        IntPtr GetCoreLibAssemblyRef(IntPtr module),
        void WriteModuleDef(IntPtr module, IntPtr path_str, int path_length),

        // TypeDef
        IntPtr GetTypeDefMethods(IntPtr type),

        // TypeRef
        IntPtr TypeRefCtor(IntPtr module, IntPtr namespace_ptr, int namespace_length, IntPtr name_ptr, int name_length, IntPtr resolution_scope),

        // TypeSig
        IntPtr SZArraySigCtor(IntPtr inner_type_sig),

        // MemberRef
        IntPtr MemberRefCtor(IntPtr module, IntPtr name_ptr, int name_length, IntPtr method_sig, IntPtr class),

        // MethodDef
        IntPtr MethodDefCtor(IntPtr name_pointer, int name_length, IntPtr signature),
        void SetMethodAttributes(IntPtr method, ushort method_attributes),
        void SetMethodImplAttributes(IntPtr method, ushort method_impl_attributes),
        IntPtr GetMethodParamDefs(IntPtr method),
        void SetMethodBody(IntPtr method, IntPtr body),

        // MethodSig
        IntPtr MethodSigCtor(int method_type, uint generic_parameter_count, IntPtr return_type, IntPtr arg_type_signatures, int arg_type_signatures_length),

        // ParamDef
        IntPtr ParamDefCtor(IntPtr name_ptr, int name_length, ushort sequence),

        // CilBody
        IntPtr CilBodyCtor(),
        IntPtr GetCilBodyInstructions(IntPtr cil_body),

        // Instruction
        IntPtr InstructionCtor(ushort opcode, IntPtr parameter),
        
        // Reflection
        IntPtr GetType(IntPtr object),
        IntPtr DynamicCall(IntPtr target, IntPtr name_ptr, int name_length, IntPtr parameters, int parameters_length),

        // Misc
        IntPtr StringCtor(IntPtr str_ptr, int str_length),
        int IListCount(IntPtr list),
        void IListAdd(IntPtr list, IntPtr element),
    }
    $
}

impl ClrAsmCtx {
    pub fn new() -> Result<Arc<ClrAsmCtx>> {
        let hostfxr = nethost::load_hostfxr().unwrap();

        let dir = std::env::current_dir().unwrap();
        let runtimeconfig_path = PdCString::try_from(dir.join("cs_sln/bin/Debug/net6.0/publish/CILEmitAPI.runtimeconfig.json").to_str().unwrap()).unwrap();
        let api_dll_path = PdCString::try_from(dir.join("cs_sln/bin/Debug/net6.0/publish/CILEmitAPI.dll").to_str().unwrap()).unwrap();
    
        let context = hostfxr.initialize_for_runtime_config(runtimeconfig_path).unwrap();
        let loader = context.get_delegate_loader_for_assembly(api_dll_path).unwrap();

        collect_functions(loader)
    }

    pub fn assembly_def(self: &Arc<Self>, name: &str, version: (i32, i32, i32, i32)) -> AssemblyDef {
        AssemblyDef::new((self.assembly_def_ctor)(name.as_ptr() as isize, name.len() as i32, version.0, version.1, version.2, version.3), self.clone())
    }

    pub fn module_def(self: &Arc<Self>, name: &str) -> ModuleDef {
        ModuleDef::new((self.module_def_ctor)(name.as_ptr() as isize, name.len() as i32), self.clone())
    }

    pub fn type_ref<S: ResolutionScope>(self: &Arc<Self>, module: &ModuleDef, namespace: &str, name: &str, scope: &S) -> TypeRef {
        TypeRef::new((self.type_ref_ctor)(module.handle(), namespace.as_ptr() as isize, namespace.len() as i32, name.as_ptr() as isize, name.len() as i32, scope.handle()), self.clone())
    }

    pub fn sz_array_sig(self: &Arc<Self>, next_sig: &TypeSig) -> TypeSig {
        TypeSig::new((self.sz_array_sig_ctor)(next_sig.handle()), self.clone())
    }

    pub fn member_ref<P: MemberRefParent>(self: &Arc<Self>, module: &ModuleDef, name: &str, signature: Option<&CallingConventionSig>, parent: Option<&P>) -> MemberRef {
        let handles = match (signature, parent) {
            (None, None) => (0, 0),
            (None, Some(_)) => panic!("You must provide a signature parameter if you provide a parent parameter."),
            (Some(CallingConventionSig::Field(f)), None) => (f.handle(), 0),
            (Some(CallingConventionSig::Method(m)), None) => (m.handle(), 0),
            (Some(CallingConventionSig::Field(f)), Some(p)) => (f.handle(), p.handle()),
            (Some(CallingConventionSig::Method(m)), Some(p)) => (m.handle(), p.handle()),
        };

        MemberRef::new((self.member_ref_ctor)(module.handle(), name.as_ptr() as isize, name.len() as i32, handles.0, handles.1), self.clone())
    }

    pub fn method_def(self: &Arc<Self>, name: &str, signature: &MethodSig) -> MethodDef {
        MethodDef::new((self.method_def_ctor)(name.as_ptr() as isize, name.len() as i32, signature.handle()), self.clone())
    }

    pub fn method_sig(self: &Arc<Self>, sig_type: MethodSigType, generic_param_count: u32, ret_type: &TypeSig, arg_types: &[&TypeSig]) -> MethodSig {
        let handles = arg_types.iter().map(|ty| ty.handle()).collect::<Vec<_>>();
        MethodSig::new(
            (self.method_sig_ctor)(sig_type as i32, generic_param_count, ret_type.handle(), handles.as_ptr() as isize, handles.len() as i32),
            self.clone()
        )
    }

    pub fn param_def(self: &Arc<Self>, name: &str, sequence: u16) -> ParamDef {
        ParamDef::new((self.param_def_ctor)(name.as_ptr() as isize, name.len() as i32, sequence), self.clone())
    }

    pub fn cil_body(self: &Arc<Self>) -> CilBody {
        CilBody::new((self.cil_body_ctor)(), self.clone())
    }

    pub fn instruction<T: ObjHandleType>(self: &Arc<Self>, code: OpCode, operand: &T) -> Instruction {
        Instruction::new((self.instruction_ctor)(code as u16, operand.handle()), self.clone())
    }

    pub fn instruction_no_operand(self: &Arc<Self>, code: OpCode) -> Instruction {
        Instruction::new((self.instruction_ctor)(code as u16, 0), self.clone())
    }

    pub fn string(self: &Arc<Self>, str: &str) -> CSString {
        CSString::new((self.string_ctor)(str.as_ptr() as isize, str.len() as i32), self.clone())
    }
}

#[macro_export]
macro_rules! instruction {
    ($ctx:expr; $opcode:ident) => {
        &$ctx.instruction_no_operand(OpCode::$opcode)
    };

    ($ctx:expr; $opcode:ident $operand:expr) => {
        &$ctx.instruction(OpCode::$opcode, &$operand)
    }
}

#[macro_export]
macro_rules! instructions {
    (
        $ctx:expr;
        $($opcode:ident $($operand:expr)?),+ $(,)?
    ) => {
        &[
            $(
                instruction!($ctx; $opcode $($operand)?),
            )+
        ]
    }
}

#[test]
fn test() {
    let _ = std::env::set_current_dir("C:/assets/programmingprojects/rust/the_rust_compiler/rust/compiler/rust_codegen_clr/crates/cil_emit_api");

    let ctx = ClrAsmCtx::new().expect("Context creation failed!");

    let mut module = ctx.module_def("hellocsharp-new");
    module.set_kind(ModuleKind::Console);

    let mut asm = ctx.assembly_def("hellocsharpnewassembly", (1, 2, 3, 4));
    asm.modules().add(&module);

    let mut entry_point = ctx.method_def(
        "Main", 
        &ctx.method_sig(
            MethodSigType::Static, 
            0,
            &module.core_lib_type(CoreLibTypes::Int32), 
            &[
                &ctx.sz_array_sig(&module.core_lib_type(CoreLibTypes::String))
            ]
        )
    );
    entry_point.set_attributes(
        MethodAttributes::Private | 
        MethodAttributes::Static | 
        MethodAttributes::HideBySig | 
        MethodAttributes::ReuseSlot
    );
    entry_point.set_impl_attributes(MethodImplAttributes::IL | MethodImplAttributes::Managed);

    entry_point.param_defs().add(&ctx.param_def("args", 1));

    module.global_type().methods().add(&entry_point);

    module.set_entry_point(&entry_point);

    let console_ref = ctx.type_ref(&module, "System", "Console", &module.core_lib_assembly_ref());
    let console_write_1 = ctx.member_ref(
        &module, 
        "WriteLine", 
        Some(&CallingConventionSig::Method(
            ctx.method_sig(
                MethodSigType::Static, 
                0, 
                &module.core_lib_type(CoreLibTypes::Void), 
                &[&module.core_lib_type(CoreLibTypes::String)]
            )
        )),
        Some(&console_ref)
    );

    let mut ep_body = ctx.cil_body();
    entry_point.set_body(&ep_body);
    ep_body.instructions().add_many(
        instructions!(
            ctx;
            Ldstr ctx.string("Hello, world!"),
            Call console_write_1,
            Ldc_I4_0,
            Ret
        )
    );

    module.write("C:/users/elect/desktop/hellocsharp-new.exe");
}