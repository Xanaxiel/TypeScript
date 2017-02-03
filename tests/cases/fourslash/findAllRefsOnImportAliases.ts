/// <reference path="fourslash.ts" />

//@Filename: a.ts
////export class [|{| "isWriteAccess": true, "isDefinition": true |}Class|] {
////}

//@Filename: b.ts
////import { [|{| "isWriteAccess": true, "isDefinition": true |}Class|] } from "./a";
////
////var c = new [|Class|]();

//@Filename: c.ts
////export { [|{| "isWriteAccess": true, "isDefinition": true |}Class|] } from "./a";

const ranges = test.ranges();
const [r0, r1, r2, r3] = ranges;
const classes = { definition: "class Class", ranges: [r0, r3] };
const imports = { definition: "import Class", ranges: [r1, r2] };
verify.referenceGroups([r0, r3], [classes, imports]);
verify.referenceGroups(r1, [imports, classes]);
verify.referenceGroups(r2, [
    { definition: "(alias) new Class(): Class\nimport Class", ranges: [r1, r2] },
    classes
]);
