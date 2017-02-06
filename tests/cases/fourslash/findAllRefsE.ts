/// <reference path='fourslash.ts' />

// @noLib: true

// @Filename: /a.ts
////export default function [|{| "isWriteAccess": true, "isDefinition": true |}f|]() {}

// @Filename: /b.ts
////import [|{| "isWriteAccess": true, "isDefinition": true |}g|], { default as [|{| "isWriteAccess": true, "isDefinition": true |}h|] } from "./a";
////[|g|](); [|h|]();

const [f, g0, h0, g1, h1] = test.ranges();
const gRanges = [g0, g1];
const hRanges = [h0, h1];
const gs = { definition: "import g", ranges: gRanges };
const hs = { definition: "import h", ranges: hRanges };
verify.referenceGroups(f, [{ definition: "function f(): void", ranges: [f] }, gs, hs]);
verify.referenceGroups(g0, [gs]);
verify.referenceGroups(h0, [hs]);
verify.referenceGroups(g1, [{ definition: "(alias) g(): void\nimport g", ranges: gRanges }]);
verify.referenceGroups(h1, [{ definition: "(alias) h(): void\nimport h", ranges: hRanges }]);
