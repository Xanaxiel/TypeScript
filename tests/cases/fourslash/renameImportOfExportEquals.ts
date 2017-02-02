/// <reference path='fourslash.ts' />
// @noLib: true

////[|declare namespace N {
////    export var x: number;
////}|]
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
const [rneg1, r0, r1, r2, r3] = test.ranges();

function verifyStuff() {
    verify.symbolAtLocation(r0, { ...r0, start: 126 }); //range is "* as N"
    verify.symbolAtLocation(r1, r1);
    verify.symbolAtLocation(r2, r2);
    verify.symbolAtLocation(r3, r2);
}
//verifyStuff();

goTo.rangeStart(r0);
//try {
    verify.renameLocations(false, false, [r0, r1, r2, r3]);
//} catch (e) {}

//verifyStuff();

/*
console.log("!");

//verify.symbolAtLocation(r3, r2);

//Leave this in, r3->r0.
//Leave it out, r3->r2.
//verify.aliasedSymbol(r1, [r0], true);

console.log("!");

//verify.symbolAtLocation(r3, r2);

//verify.symbolAtLocation(r1, r1);
//verify.symbolAtLocation(r2, r2);

//Something that I do in  findAllReferences.ts makes r3 -> r0 instead....
//verify.symbolAtLocation(r3, r2);

*/