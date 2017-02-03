/// <reference path="fourslash.ts" />

//@Filename: a.ts
////export class [|{| "isWriteAccess": true, "isDefinition": true |}Class|] {
////}

//@Filename: b.ts
////import { [|{| "isWriteAccess": true, "isDefinition": true |}Class|] as [|{| "isWriteAccess": true, "isDefinition": true |}C2|] } from "./a";
////
////var c = new [|C2|]();

//@Filename: c.ts
////export { [|{| "isWriteAccess": true, "isDefinition": true |}Class|] as [|{| "isWriteAccess": true, "isDefinition": true |}C3|] } from "./a";

const ranges = test.rangesByText();
const [class0, class1, class2] = ranges.get("Class");
const c2Ranges = ranges.get("C2");
const [c2_0, c2_1] = c2Ranges;
const c2s =  { definition: "import C2", ranges: [c2_0, c2_1] };

const classes = { definition: "class Class", ranges: [class0, class2] };

verify.referenceGroups([class0, class2], [classes, c2s]);
verify.referenceGroups(class1, [{ definition: "import C2", ranges: [class1] }]);

verify.referenceGroups(c2_0, [c2s])
verify.referenceGroups(c2_1, [{ definition: "(alias) new C2(): C2\nimport C2", ranges: c2Ranges }]);

verify.singleReferenceGroup("import C3", ranges.get("C3"));
