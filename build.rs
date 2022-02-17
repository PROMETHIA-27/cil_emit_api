fn main() {
    println!("cargo:rerun-if-changed=cs_sln/EmitAPI.cs");
    println!("{:?}", std::process::Command::new("cmd").args(["/C", "cd cs_sln && build && cd .."]).output().expect("Failed to build c# project"));
}