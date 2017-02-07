/// <reference path='fourslash.ts' />
// @noLib: true

////declare namespace [|{| "isWriteAccess": true, "isDefinition": true |}N|] {
////    export var x: number;
////}
////declare module "mod" {
////    export = [|N|];
////}
////declare module "a" {
////    import * as [|{| "isWriteAccess": true, "isDefinition": true |}N|] from "mod";
////    export { [|{| "isWriteAccess": true, "isDefinition": true |}N|] }; // Renaming N here would rename
////}
////declare module "b" {
////    import { [|{| "isWriteAccess": true, "isDefinition": true |}N|] } from "a";
////    export const y: typeof [|N|].x;
////}

//TODO: version of this test where `import { N }` -> `import { N as M }`

const ranges = test.ranges();
const [N0, N1, a0, a1, b0, b1] = ranges;
const nRanges = [N0, N1];
const aRanges = [a0, a1];
const bRanges = [b0, b1];

const nGroup = { definition: "namespace N", ranges: nRanges };
const aGroup = { definition: "import N", ranges: aRanges };
const bGroup = { definition: "import N", ranges: [b0, b1] };

verify.referenceGroups(nRanges, [nGroup]);
verify.referenceGroups([a0, a1], [aGroup, nGroup, bGroup]);
verify.referenceGroups(bRanges, [bGroup, aGroup, nGroup]);

verify.rangesAreRenameLocations(false, false, nRanges);
verify.rangesAreRenameLocations(false, false, aRanges.concat(bRanges));
