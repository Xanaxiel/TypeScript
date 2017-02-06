/// <reference path='fourslash.ts' />

// @noLib: true

// @Filename: /a.ts
////var [|{| "isWriteAccess": true, "isDefinition": true |}a|];
////export { [|{| "isWriteAccess": true, "isDefinition": true |}a|] };

// @Filename: /b.ts
////import { [|{| "isWriteAccess": true, "isDefinition": true |}a|] } from "./a";
////[|a|];

verify.noErrors();

const [a0, a1, b0, b1] = test.ranges();
const aRefs = [a0, a1], bRefs = [b0, b1];
const vars = { definition: "var a: any", ranges: aRefs };
const imports = { definition: "import a", ranges: bRefs };
verify.referenceGroups(aRefs, [vars, imports]);
verify.referenceGroups(bRefs, [imports, vars]);
