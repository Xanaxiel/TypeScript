/// <reference path='fourslash.ts' />
// @noLib: true

////declare namespace N {
////    export var x: number;
////}
////declare module "mod" {
////    export = N;
////}
////declare module "test" {
////    import * as [|N|] from "mod";
////    export { [|N|] }; // Renaming N here would rename
////}
////declare module "test2" {
////    import { [|N|] } from "test";
////    export const y: typeof [|N|].x;
////}

//TODO: version of this test where `import { N }` -> `import { N as M }`

//verify.rangesAreRenameLocations();
const ranges = test.ranges();
const [r0, r1, r2, r3] = test.ranges();
goTo.rangeStart(r0);
verify.renameLocations(false, false, ranges);
