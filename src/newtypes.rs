use std::sync::Arc;

use crate::ClrAsmCtx;

use super::enums::*;

pub struct ObjHandle {
    pub(crate) handle: isize, 
    pub(crate) ctx: Arc<crate::ClrAsmCtx>
}
impl Drop for ObjHandle {
    fn drop(&mut self) {
        (self.ctx.free_handle)(self.handle)
    }
}

macro_rules! obj_handle_type {
    ($name:ident) => {
        pub struct $name(pub(crate) ObjHandle);
        impl $name {
            pub fn new(handle: isize, ctx: Arc<crate::ClrAsmCtx>) -> Self {
                $name(ObjHandle { handle, ctx })
            }
        }
        impl private::Sealed for $name {}
        impl ObjHandleType for $name {
            fn handle(&self) -> isize {
                self.0.handle
            }
            fn ctx(&self) -> &Arc<ClrAsmCtx> {
                &self.0.ctx
            }
        }
    };

    ($name:ident<$($gen_params:ident $(: $traits:path)?)+>) => {
        #[allow(unused_parens)]
        pub struct $name<$($gen_params $(: $traits)?)+>(pub(crate) ObjHandle, std::marker::PhantomData<($($gen_params)+)>);
        impl<$($gen_params $(: $traits)?)+> $name<$($gen_params)+> {
            fn new(handle: isize, ctx: Arc<crate::ClrAsmCtx>) -> Self {
                $name(ObjHandle { handle, ctx }, Default::default())
            }
        }
        impl<$($gen_params $(: $traits)?)+> private::Sealed for $name<$($gen_params)+> {}
        impl<$($gen_params $(: $traits)?)+> ObjHandleType for $name<$($gen_params)+> {
            fn handle(&self) -> isize {
                self.0.handle
            }
            fn ctx(&self) -> &Arc<ClrAsmCtx> {
                &self.0.ctx
            }
        }
    }
}

mod private {
    pub trait Sealed {}
}

pub trait ObjHandleType : private::Sealed {
    fn handle(&self) -> isize;
    fn ctx(&self) -> &Arc<ClrAsmCtx>;
}

pub trait ResolutionScope : ObjHandleType {}

obj_handle_type!(AssemblyDef);
impl AssemblyDef {
    pub fn modules(&mut self) -> IList<ModuleDef> {
        IList::new((self.ctx().get_assembly_modules)(self.handle()), self.ctx().clone())
    }
}

obj_handle_type!(AssemblyRef);
impl ResolutionScope for AssemblyRef {}

obj_handle_type!(ModuleDef);
impl ModuleDef {
    pub fn set_kind(&mut self, kind: ModuleKind) {
        (self.ctx().set_module_kind)(self.handle(), kind as i32)
    }

    pub fn set_entry_point(&mut self, method: &MethodDef) {
        (self.ctx().set_module_entry_point)(self.handle(), method.handle())
    }

    pub fn global_type(&mut self) -> TypeDef {
        TypeDef::new((self.ctx().get_global_type)(self.handle()), self.ctx().clone())
    }

    pub fn core_lib_type(&self, ty: CoreLibTypes) -> TypeSig {
        TypeSig::new((self.ctx().get_core_lib_type)(self.handle(), ty as i32), self.ctx().clone())
    }

    pub fn core_lib_assembly_ref(&self) -> AssemblyRef {
        AssemblyRef::new((self.ctx().get_core_lib_assembly_ref)(self.handle()), self.ctx().clone())
    }

    pub fn types(&mut self) -> IList<TypeDef> {
        IList::new((self.ctx().get_module_types)(self.handle()), self.ctx().clone())
    }

    pub fn write(&self, path: &str) {
        (self.ctx().write_module_def)(self.handle(), path.as_ptr() as isize, path.len() as i32)
    }
}

obj_handle_type!(TypeDef);
impl TypeDef {
    pub fn methods(&mut self) -> IList<MethodDef> {
        IList::new((self.ctx().get_type_def_methods)(self.handle()), self.ctx().clone())
    }
}

obj_handle_type!(TypeRef);
impl MemberRefParent for TypeRef {}
impl TypeRef {

}

pub trait TypeDefOrRef : ObjHandleType {}
impl TypeDefOrRef for TypeDef {}
impl TypeDefOrRef for TypeRef {}

obj_handle_type!(TypeSig);

obj_handle_type!(MemberRef);

pub trait MemberRefParent : ObjHandleType {}

pub enum CallingConventionSig {
    Method(MethodSig),
    Field(FieldSig),
}

obj_handle_type!(MethodDef);
impl MethodDef {
    pub fn set_attributes(&mut self, attributes: MethodAttributes) {
        (self.ctx().set_method_attributes)(self.handle(), attributes.bits())
    }

    pub fn set_impl_attributes(&mut self, attributes: MethodImplAttributes) {
        (self.ctx().set_method_impl_attributes)(self.handle(), attributes.bits())
    }

    pub fn param_defs(&mut self) -> IList<ParamDef> {
        IList::new((self.ctx().get_method_param_defs)(self.handle()), self.ctx().clone())
    }

    pub fn set_body(&mut self, cil_body: &CilBody) {
        (self.ctx().set_method_body)(self.handle(), cil_body.handle())
    }
}

obj_handle_type!(MethodSig);

obj_handle_type!(ParamDef);

obj_handle_type!(FieldSig);

obj_handle_type!(CilBody);
impl CilBody {
    pub fn instructions(&mut self) -> IList<Instruction> {
        IList::new((self.ctx().get_cil_body_instructions)(self.handle()), self.ctx().clone())
    }

    pub fn locals(&mut self) -> IList<Local> {
        IList::new((self.ctx().get_cil_body_locals)(self.handle()), self.ctx().clone())
    }
}

obj_handle_type!(Local);

obj_handle_type!(Instruction);

obj_handle_type!(CSString);

obj_handle_type!(IList<T: ObjHandleType>);
impl<T: ObjHandleType> IList<T> {
    pub fn add(&mut self, element: &T) {
        (self.ctx().i_list_add)(self.handle(), element.handle())
    }

    pub fn add_many(&mut self, elements: &[&T]) {
        for elem in elements {
            self.add(elem)
        }
    }
}