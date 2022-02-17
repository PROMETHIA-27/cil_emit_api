using System.Collections;
using System.Runtime.InteropServices;

using dnlib.DotNet;
using dnlib.DotNet.Emit;

namespace CILEmitAPI;

public static class EmitAPI
{
    static T GetObject<T>(IntPtr handle) => (T)GCHandle.FromIntPtr(handle).Target!;

    static IntPtr GetHandle<T>(T @object) => GCHandle.ToIntPtr(GCHandle.Alloc(@object));

    static string GetString(IntPtr ptr, int len) => Marshal.PtrToStringUTF8(ptr, len);

    static T[] GetArray<T>(IntPtr arr_ptr, int len)
    {
        ReadOnlySpan<IntPtr> handles;
        unsafe {
            handles = new ReadOnlySpan<IntPtr>((void*)arr_ptr, len);
        }
        T[] arr = new T[len];
        for (int i = 0; i < len; i++)
            arr[i] = GetObject<T>(handles[i]);

        return arr;
    }

    static object? CallMethod(object? target, string name, params object?[] parameters)
    {
        return target?.GetType().GetMethod(name, 
        System.Reflection.BindingFlags.Public | 
        System.Reflection.BindingFlags.NonPublic | 
        System.Reflection.BindingFlags.Static | 
        System.Reflection.BindingFlags.Instance
        )?.Invoke(target, parameters);
    }

    [UnmanagedCallersOnly]
    public static void FreeHandle(IntPtr handle)
    {
        GCHandle.FromIntPtr(handle).Free();
    }

#region AssemblyDef
    [UnmanagedCallersOnly]
    public static IntPtr AssemblyDefCtor(IntPtr name, int name_length, int vers_major, int vers_minor, int vers_build, int vers_revision)
    {
        string str_name = GetString(name, name_length);
        Version vers = new Version(vers_major, vers_minor, vers_build, vers_revision);
        AssemblyDefUser asm_def = new(str_name, vers, null, "en");
        return GetHandle(asm_def);
    }

    [UnmanagedCallersOnly]
    public static IntPtr GetAssemblyModules(IntPtr assembly) 
    {
        AssemblyDef asm = GetObject<AssemblyDef>(assembly);
        return GetHandle(asm.Modules);
    }
#endregion

#region ModuleDef
    [UnmanagedCallersOnly]
    public static IntPtr ModuleDefCtor(IntPtr name_ptr, int name_length) => GetHandle(new ModuleDefUser(GetString(name_ptr, name_length)));

    [UnmanagedCallersOnly]
    public static void SetModuleKind(IntPtr module, int kind)
    {
        ModuleDef mod = GetObject<ModuleDef>(module);
        mod.Kind = (ModuleKind)kind;
    }

    [UnmanagedCallersOnly]
    public static void SetModuleEntryPoint(IntPtr module, IntPtr method)
    {
        ModuleDef mod = GetObject<ModuleDef>(module);
        MethodDef meth = GetObject<MethodDef>(method);
        mod.EntryPoint = meth;
    }

    [UnmanagedCallersOnly]
    public static IntPtr GetGlobalType(IntPtr module)
    {
        ModuleDef mod = GetObject<ModuleDef>(module);
        return GetHandle(mod.GlobalType);
    }

    enum CoreLibTypes : int
    {
        UInt32 = 0,
        UIntPtr = 1,
        IntPtr = 2,
        TypedReference = 3,
        String = 4,
        Double = 5,
        Single = 6,
        UInt64 = 7,
        Int64 = 8,
        Int32 = 9,
        UInt16 = 10,
        Int16 = 11,
        Byte = 12,
        SByte = 13,
        Char = 14,
        Boolean = 15,
        Void = 16,
        Object = 17,
    }

    [UnmanagedCallersOnly]
    public static IntPtr GetCoreLibType(IntPtr module, int core_lib_type) 
    {
        CoreLibTypes ty = (CoreLibTypes)core_lib_type;
        ModuleDef mod = GetObject<ModuleDef>(module);

        CorLibTypeSig sig = ty switch {
            CoreLibTypes.UInt32 => mod.CorLibTypes.UInt32,
            CoreLibTypes.UIntPtr => mod.CorLibTypes.UIntPtr,
            CoreLibTypes.IntPtr => mod.CorLibTypes.IntPtr,
            CoreLibTypes.TypedReference => mod.CorLibTypes.TypedReference,
            CoreLibTypes.String => mod.CorLibTypes.String,
            CoreLibTypes.Double => mod.CorLibTypes.Double,
            CoreLibTypes.Single => mod.CorLibTypes.Single,
            CoreLibTypes.UInt64 => mod.CorLibTypes.UInt64,
            CoreLibTypes.Int64 => mod.CorLibTypes.Int64,
            CoreLibTypes.Int32 => mod.CorLibTypes.Int32,
            CoreLibTypes.UInt16 => mod.CorLibTypes.UInt16,
            CoreLibTypes.Int16 => mod.CorLibTypes.Int16,
            CoreLibTypes.Byte => mod.CorLibTypes.Byte,
            CoreLibTypes.SByte => mod.CorLibTypes.SByte,
            CoreLibTypes.Char => mod.CorLibTypes.Char,
            CoreLibTypes.Boolean => mod.CorLibTypes.Boolean,
            CoreLibTypes.Void => mod.CorLibTypes.Void,
            CoreLibTypes.Object => mod.CorLibTypes.Object,
            _ => throw new Exception("Invalid corlibtype")
        };

        return GetHandle(sig);
    }

    [UnmanagedCallersOnly]
    public static IntPtr GetCoreLibAssemblyRef(IntPtr module) 
    {
        ModuleDef mod = GetObject<ModuleDef>(module);
        return GetHandle(mod.CorLibTypes.AssemblyRef);
    }

    [UnmanagedCallersOnly]
    public static void WriteModuleDef(IntPtr module, IntPtr path_str, int path_length)
    {
        string path = GetString(path_str, path_length);
        ModuleDef mod = GetObject<ModuleDef>(module);
        mod.Write(path);
    }
#endregion

#region TypeDef
    [UnmanagedCallersOnly]
    public static IntPtr GetTypeDefMethods(IntPtr type)
    {
        TypeDef ty = GetObject<TypeDef>(type);
        return GetHandle(ty.Methods);
    }
#endregion

#region TypeRef
    [UnmanagedCallersOnly]
    public static IntPtr TypeRefCtor(IntPtr module, IntPtr namespace_ptr, int namespace_length, IntPtr name_ptr, int name_length, IntPtr resolution_scope)
    {
        ModuleDef mod = GetObject<ModuleDef>(module);
        string ns = GetString(namespace_ptr, namespace_length);
        string name = GetString(name_ptr, name_length);
        IResolutionScope scope = GetObject<IResolutionScope>(resolution_scope);
        return GetHandle(new TypeRefUser(mod, ns, name, scope));
    }
#endregion

#region TypeSig
    [UnmanagedCallersOnly]
    public static IntPtr SZArraySigCtor(IntPtr inner_type_sig)
    {
        TypeSig sig = GetObject<TypeSig>(inner_type_sig);
        return GetHandle(new SZArraySig(sig));
    }
#endregion

#region MemberRef
    [UnmanagedCallersOnly]
    public static IntPtr MemberRefCtor(IntPtr module, IntPtr name_ptr, int name_length, IntPtr member_sig, IntPtr @class)
    {
        ModuleDef mod = GetObject<ModuleDef>(module);
        string name = GetString(name_ptr, name_length);
        CallingConventionSig? sig = member_sig != null ? GetObject<CallingConventionSig>(member_sig) : null;
        IMemberRefParent? parent = @class != null ? GetObject<IMemberRefParent>(@class) : null;
        return GetHandle((sig, parent) switch {
            (null, null) => new MemberRefUser(mod, name),
            (FieldSig f, null) => new MemberRefUser(mod, name, f),
            (FieldSig f, var p) => new MemberRefUser(mod, name, f, p),
            (MethodSig m, null) => new MemberRefUser(mod, name, m),
            (MethodSig m, var p) => new MemberRefUser(mod, name, m, p),
            _ => throw new Exception("Invalid MemberRefCtor parameters"),
        });
    }
#endregion

#region MethodDef
    [UnmanagedCallersOnly]
    public static IntPtr MethodDefCtor(IntPtr name_pointer, int name_length, IntPtr signature)
    {
        string name = GetString(name_pointer, name_length);
        MethodSig sig = GetObject<MethodSig>(signature);

        return GetHandle(new MethodDefUser(name, sig));
    }

    [UnmanagedCallersOnly]
    public static void SetMethodAttributes(IntPtr method, ushort method_attributes)
    {
        MethodDef meth = GetObject<MethodDef>(method);
        MethodAttributes attrs = (MethodAttributes)method_attributes;
        meth.Attributes = attrs;
    }

    [UnmanagedCallersOnly]
    public static void SetMethodImplAttributes(IntPtr method, ushort method_impl_attributes)
    {
        MethodDef meth = GetObject<MethodDef>(method);
        MethodImplAttributes attrs = (MethodImplAttributes)method_impl_attributes;
        meth.ImplAttributes = attrs;
    }

    [UnmanagedCallersOnly]
    public static IntPtr GetMethodParamDefs(IntPtr method)
    {
        MethodDef meth = GetObject<MethodDef>(method);
        return GetHandle(meth.ParamDefs);
    }

    [UnmanagedCallersOnly]
    public static void SetMethodBody(IntPtr method, IntPtr body)
    {
        MethodDef meth = GetObject<MethodDef>(method);
        CilBody bod = GetObject<CilBody>(body);
        meth.Body = bod;
    }
#endregion

#region MethodSig
    enum MethodSigType : int
    {
        Static,
        Instance,
    }

    [UnmanagedCallersOnly]
    public static IntPtr MethodSigCtor(int method_type, uint generic_parameter_count, IntPtr return_type, IntPtr arg_type_signatures, int arg_type_signatures_length)
    {
        var ret_ty = GetObject<TypeSig>(return_type);
        var arg_sigs = GetArray<TypeSig>(arg_type_signatures, arg_type_signatures_length);

        var method_sig = (MethodSigType)method_type switch {
            MethodSigType.Static when generic_parameter_count == 0 => MethodSig.CreateStatic(ret_ty, arg_sigs),
            MethodSigType.Instance when generic_parameter_count == 0  => MethodSig.CreateInstance(ret_ty, arg_sigs),
            MethodSigType.Static => MethodSig.CreateStaticGeneric(generic_parameter_count, ret_ty, arg_sigs),
            MethodSigType.Instance => MethodSig.CreateInstanceGeneric(generic_parameter_count, ret_ty, arg_sigs),
            _ => throw new Exception("Invalid methodsigtype"),
        };

        return GetHandle(method_sig);
    }
#endregion

#region ParamDef
    [UnmanagedCallersOnly]
    public static IntPtr ParamDefCtor(IntPtr name_ptr, int name_length, ushort sequence) 
    {
        string name = GetString(name_ptr, name_length);
        ParamDefUser param = new ParamDefUser(name, sequence);
        return GetHandle(param);
    }
#endregion

#region CilBody
    [UnmanagedCallersOnly]
    public static IntPtr CilBodyCtor()
    {
        return GetHandle(new CilBody());
    }

    [UnmanagedCallersOnly]
    public static IntPtr GetCilBodyInstructions(IntPtr cil_body)
    {
        CilBody bod = GetObject<CilBody>(cil_body);
        return GetHandle(bod.Instructions);
    }
#endregion

#region Instruction
    [UnmanagedCallersOnly]
    public static IntPtr InstructionCtor(ushort opcode, IntPtr parameter)
    {
        Code opc = (Code)opcode;
        if (parameter != IntPtr.Zero) // 0'd GCHandles are invalid, so I can use it as optional
            return GetHandle(new Instruction(opc.ToOpCode(), GetObject<object>(parameter)));
        else 
            return GetHandle(new Instruction(opc.ToOpCode())); 
    }
#endregion

#region Reflection
    [UnmanagedCallersOnly]
    public static IntPtr GetType(IntPtr @object)
    {
        object obj = GetObject<object>(@object);
        return GetHandle(obj.GetType());
    }

    // TODO: Figure out passing strings back to rust

    [UnmanagedCallersOnly]
    public static IntPtr DynamicCall(IntPtr target, IntPtr name_ptr, int name_length, IntPtr parameters, int parameters_length)
    {
        object targ = GetObject<object>(target);
        string name = GetString(name_ptr, name_length);
        object?[] param = GetArray<object?>(parameters, parameters_length);
        return GetHandle(CallMethod(targ, name, param));
    }
#endregion

#region Misc
    [UnmanagedCallersOnly]
    public static IntPtr StringCtor(IntPtr str_ptr, int str_length)
    {
        return GetHandle(GetString(str_ptr, str_length));
    }

    [UnmanagedCallersOnly]
    public static int IListCount(IntPtr list) 
    {
        return (int)CallMethod(GetObject<object>(list), "Count")!;
    }

    [UnmanagedCallersOnly]
    public static void IListAdd(IntPtr list, IntPtr element) 
    {
        object ls = GetObject<object>(list);
        object elem = GetObject<object>(element);
        CallMethod(ls, "Add", elem);
    }
#endregion
}