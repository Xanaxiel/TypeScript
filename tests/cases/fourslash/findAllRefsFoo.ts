/// <reference path='fourslash.ts' />

// @noLib: true

// @Filename: /a.ts
////var [|{| "isWriteAccess": true, "isDefinition": true |}a|];
////export { [|{| "isWriteAccess": true, "isDefinition": true |}a|] };

// @Filename: /b.ts
////import { [|{| "isWriteAccess": true, "isDefinition": true |}a|] } from "./a";
////[|a|];

verify.numberOfErrorsInCurrentFile(0);

const [r0, r1, r2, r3] = test.ranges();
verify.referenceGroups(r0, [
    { definition: "var a: any", ranges: [r0, r1] },
    { definition: "import a", ranges: [r2, r3] }
]);
//TODO:MOAR TESTS
