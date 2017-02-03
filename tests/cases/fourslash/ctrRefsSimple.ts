/// <reference path="fourslash.ts" />

// @Filename: a.ts
////export class C {
////    [|constructor|](){}
////}

// @Filename: b.ts
////import { C } from "./a";
////new [|C|]();

const ranges = test.ranges();
const [r0, r1] = ranges;
verify.referenceGroups(r0, [
    { definition: "constructor C(): C", ranges: [r0] },
    { definition: "import C", ranges: [r1] }
]);
