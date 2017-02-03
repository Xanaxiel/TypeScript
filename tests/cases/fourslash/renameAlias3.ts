/// <reference path='fourslash.ts'/>

////module SomeModule { export class [|SomeClass|] { } }
////import M = SomeModule;
////import C = M.[|SomeClass|];

//verify.rangesAreRenameLocations();
const [r0, r1] = test.ranges();
goTo.rangeStart(r0);
verify.renameLocations(false, false, [r0, r1]);
