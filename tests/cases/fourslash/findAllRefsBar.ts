/// <reference path='fourslash.ts' />

// @noLib: true

// @Filename: /a.ts
////export default function [|{| "isWriteAccess": true, "isDefinition": true |}f|]() {}
////[|f|]();

// @Filename: /b.ts
////import [|{| "isWriteAccess": true, "isDefinition": true |}f|] from "./a";
////[|f|]();

verify.noErrors();

const [a0, a1, b0, b1] = test.ranges();
const aRefs = [a0, a1], bRefs = [b0, b1];
const fs = { definition: "function f(): void", ranges: aRefs };
const imports1 = { definition: "import f", ranges: bRefs }
const imports2 = { definition: "(alias) f(): void\nimport f", ranges: bRefs }
verify.referenceGroups(aRefs, [fs, imports1]);
verify.referenceGroups(b0, [imports1, fs]);
verify.referenceGroups(b1, [imports2, fs]);
