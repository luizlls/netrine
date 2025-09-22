use wasmer::{Store, Module, Instance, Value, imports};

pub fn eval(file_path: String, source: &str) -> anyhow::Result<String> {
    let source = netrine::source(file_path, source);
    let wasm = netrine::compile(&source)?;

    let mut store = Store::default();

    let module = Module::new(&store, &wasm)?;
    let import = imports! {};
    let instance = Instance::new(&mut store, &module, &import)?;

    let main_fn = instance.exports.get_function("main")?;

    let result = match main_fn.call(&mut store, &[])?[0] {
        Value::I32(value) => value.to_string(),
        Value::I64(value) => value.to_string(),
        Value::F32(value) => value.to_string(),
        Value::F64(value) => value.to_string(),
        _ => unimplemented!()
    };

    Ok(result)
}
