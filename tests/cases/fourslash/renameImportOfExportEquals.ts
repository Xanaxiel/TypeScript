/// <reference path='fourslash.ts' />
// @noLib: true

////declare namespace N {
////    export var x: number;
////}
////declare module "mod" {
////    export = N;
////}
////declare module "test" {
////    import * as [|{| "isWriteAccess": true, "isDefinition": true |}N|] from "mod";
////    export { [|{| "isWriteAccess": true, "isDefinition": true |}N|] }; // Renaming N here would rename
////}
////declare module "test2" {
////    import { [|{| "isWriteAccess": true, "isDefinition": true |}N|] } from "test";
////    export const y: typeof [|N|].x;
////}

//TODO: version of this test where `import { N }` -> `import { N as M }`

verify.rangesAreRenameLocations();

const ranges = test.ranges();
const [r0, r1, r2, r3] = ranges;
const test1 = [r0, r1];
const test2 = [r2, r3];
const group1 = { definition: "import N", ranges: test1 };
const group2 = { definition: "import N", ranges: test2 };
verify.referenceGroups(test1, [group1, group2]);
verify.referenceGroups(test2, [group2, group1]);
