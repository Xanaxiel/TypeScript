/// <reference path='fourslash.ts' />

// @Filename: a.ts
////export var [|{| "isWriteAccess": true, "isDefinition": true |}a|];

// @Filename: b.ts
////import { [|{| "isWriteAccess": true, "isDefinition": true |}a|] } from './a';
////export { [|{| "isWriteAccess": true, "isDefinition": true |}a|] };

const ranges = test.ranges();
const [r0, r1, r2] = ranges;
verify.referenceGroups([r0, r2], [{ definition: "var a: any", ranges }]);
verify.referenceGroups(r1, [
    { definition: "import a", ranges: [r1, r2] },
    { definition: "var a: any", ranges: [r0] }
]);
verify.rangesAreRenameLocations();
