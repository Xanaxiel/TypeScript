/// <reference path='fourslash.ts' />
// @noLib: true

////declare module "a" {
////    export class [|{| "isWriteAccess": true, "isDefinition": true |}C|] {}
////}
////declare module "b" {
////    export { [|{| "isWriteAccess": true, "isDefinition": true |}C|] } from "a";
////}
////declare module "c" {
////    import { [|{| "isWriteAccess": true, "isDefinition": true |}C|] } from "b";
////    export function f(c: [|C|]): void;
////}

//TODO: version of this test where `import { N }` -> `import { N as M }`

verify.noErrors();

verify.rangesAreRenameLocations();

const ranges = test.ranges();
const [r0, r1, r2, r3] = ranges;
const classRanges = [r0, r1];
const importRanges = [r2, r3];
const classes = { definition: "class C", ranges: classRanges };
const imports = { definition: "import C", ranges: importRanges };
verify.referenceGroups(classRanges, [classes, imports]);
verify.referenceGroups(importRanges, [imports, classes]);
