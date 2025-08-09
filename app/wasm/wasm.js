const NETRINE_MODULE = new URL('wasm.wasm', import.meta.url);

const USIZE = 4;
const SLICE_SIZE = USIZE << 1;

const ENCODER = new TextEncoder();
const DECODER = new TextDecoder();

function allocTarget(instance) {
  return instance.alloc(SLICE_SIZE, USIZE);
}

function allocSlice(instance, sourcePointer, sourceLength, align) {
  const ptr = instance.alloc(SLICE_SIZE, align);

  const view = new DataView(instance.memory.buffer);
  view.setUint32(ptr, sourcePointer, true);
  view.setUint32(ptr + USIZE, sourceLength, true);

  return ptr;
}

function allocString(instance, string) {
  const buf = ENCODER.encode(string);
  const ptr = instance.alloc(buf.length, 1);

  const memory = new Uint8Array(instance.memory.buffer);
  memory.set(buf, ptr);

  return allocSlice(instance, ptr, buf.length, 1);
}

function getResult(instance, resultSlice) {
  const view = new DataView(instance.memory.buffer);
  const ptr = view.getUint32(resultSlice, true);
  const len = view.getUint32(resultSlice + USIZE, true);

  return new Uint8Array(instance.memory.buffer, ptr, len);
}

export async function compile(netrine, source) {
  const sourceSlice = allocString(netrine, source);
  const resultSlice = allocTarget(netrine);
  netrine.compile(sourceSlice, resultSlice);
  const result = getResult(netrine, resultSlice);
  const module = await WebAssembly.instantiate(result);
  return module.instance.exports.main();
}

export async function initialize() {
  const wasm = await WebAssembly.instantiateStreaming(fetch(NETRINE_MODULE));
  return wasm.instance.exports;
}
