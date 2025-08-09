const NETRINE_MODULE = new URL('wasm.wasm', import.meta.url);

const USIZE = 4;
const SLICE_SIZE = USIZE << 1;
const SLICE_ALIGN = 1;

const RESULT_SIZE = USIZE << 1;
const RESULT_ALIGN = 4;

const ENCODER = new TextEncoder();
const DECODER = new TextDecoder();

function allocSlice(instance, sourcePointer, sourceLength) {
  const ptr = instance.allocate(SLICE_SIZE, SLICE_ALIGN);

  const view = new DataView(instance.memory.buffer);
  view.setUint32(ptr, sourcePointer, true);
  view.setUint32(ptr + USIZE, sourceLength, true);

  return ptr;
}

function getSliceData(instance, slicePointer) {
  const view = new DataView(instance.memory.buffer);
  const ptr = view.getUint32(slicePointer, true);
  const len = view.getUint32(slicePointer + USIZE, true);

  return { ptr, len };
}

function allocString(instance, string) {
  const buf = ENCODER.encode(string);
  const ptr = instance.allocate(buf.length, SLICE_ALIGN);

  const memory = new Uint8Array(instance.memory.buffer);
  memory.set(buf, ptr);

  return allocSlice(instance, ptr, buf.length);
}

function freeString(instance, slicePointer) {
  const { ptr, len } = getSliceData(instance, slicePointer);
  instance.deallocate(ptr, len, SLICE_ALIGN);
  instance.deallocate(slicePointer, SLICE_SIZE, SLICE_ALIGN);
}

function allocResult(instance) {
  return instance.allocate(RESULT_SIZE, RESULT_ALIGN);
}

function freeResult(instance, resultPointer) {
  instance.free_result(resultPointer);
  instance.deallocate(resultPointer, RESULT_SIZE, RESULT_ALIGN);
}

function getResultData(instance, resultPointer) {
  const view = new DataView(instance.memory.buffer);
  const ptr = view.getUint32(resultPointer, true);
  const len = view.getUint32(resultPointer + USIZE, true);

  return { ptr, len };
}

function getResultValue(instance, resultPointer) {
  const { ptr, len } = getResultData(instance, resultPointer);
  return new Uint8Array(instance.memory.buffer, ptr, len);
}

export async function compile(netrine, source) {
  const slicePointer = allocString(netrine, source);
  const resultPointer = allocResult(netrine);
  netrine.compile(slicePointer, resultPointer);

  const result = getResultValue(netrine, resultPointer);
  const module = await WebAssembly.instantiate(result);

  freeString(netrine, slicePointer);
  freeResult(netrine, resultPointer);

  return module.instance.exports;
}

export async function initialize() {
  const wasm = await WebAssembly.instantiateStreaming(fetch(NETRINE_MODULE));
  return wasm.instance.exports;
}
