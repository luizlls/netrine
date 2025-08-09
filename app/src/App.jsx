import "./App.css";
import { useCallback, useEffect, useRef, useState } from "react";
import { Editor } from "./components/Editor";

import { initialize, compile } from "../wasm/wasm";

function App() {
  const containerRef = useRef(null);

  const [netrine, setNetrine] = useState(undefined);
  const [header, setHeader] = useState("Netrine");

  useEffect(() => {
    async function setup() {
      const netrine = await initialize();
      setNetrine(netrine);
    }
    setup();
  });

  const onChange = useCallback(
    async (code) => {
      setHeader(await compile(netrine, code));
    },
    [netrine],
  );

  return (
    <div className="app-container" ref={containerRef}>
      <Editor
        header={header}
        x={100}
        y={50}
        containerRef={containerRef}
        onChange={onChange}
      />
    </div>
  );
}

export default App;
