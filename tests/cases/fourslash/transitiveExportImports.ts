/// <reference path='fourslash.ts'/>

// @Filename: a.ts
////class [|{| "isWriteAccess": true, "isDefinition": true |}A|] {
////}
////export = [|A|];

// @Filename: b.ts
////export import [|{| "isWriteAccess": true, "isDefinition": true |}b|] = require('./a');

// @Filename: c.ts
////import [|{| "isWriteAccess": true, "isDefinition": true |}b|] = require('./b');
////var a = new [|b|]./**/[|b|]();

goTo.marker();
verify.quickInfoExists();
verify.noErrors();

const ranges = test.ranges();
const [a0, a1, b0, c0, c1, c2] = ranges;
const aRanges = [a0, a1];
const bRanges = [b0];

const bGroup = { definition: "import b = require('./a')", ranges: [b0, c2] }

verify.referenceGroups([a0, a1], [
    { definition: "class A", ranges: aRanges },
    bGroup
]);
verify.referenceGroups(b0, [bGroup]);
verify.referenceGroups([c0, c1], [{ definition: "import b = require('./b')", ranges: [c0, c1] }]);
verify.referenceGroups(c2, [{ ...bGroup, definition: "(alias) new b.b(): b.b\nimport b.b = require('./a')"}]);

verify.rangesAreRenameLocations(false, false, [a0, a1]);
verify.rangesAreRenameLocations(false, false, [b0, c2]);
verify.rangesAreRenameLocations(false, false, [c0, c1]);
