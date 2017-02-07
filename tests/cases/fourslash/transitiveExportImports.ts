/// <reference path='fourslash.ts'/>

// @Filename: a.ts
////class [|A|] {
////}
////export = [|A|];

// @Filename: b.ts
////export import [|b|] = require('./a');

// @Filename: c.ts
////import [|b|] = require('./b');
////var a = new [|b|]./**/[|b|]();

goTo.marker();
verify.quickInfoExists();
verify.noErrors();

const ranges = test.ranges();
const [a0, a1, b0, b1, b2] = ranges;
verify.referenceGroups(a0, [{ definition: "class A", ranges }]);
verify.referenceGroups(a1, []);
verify.referenceGroups(b0, []);
verify.referenceGroups(b1, []);
verify.referenceGroups(b2, []);

