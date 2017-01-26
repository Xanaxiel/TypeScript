/// <reference path='fourslash.ts' />

// @noLib: true

// @Filename: /a.ts
////var [|a|];
////export { [|a|] };

// @Filename: /b.ts
////import { [|a|] } from "./a";
////[|a|];


verify.numberOfErrorsInCurrentFile(0);

const [r0, r1, r2] = test.ranges();
verify.referencesOf(r0, [r0, r1, r2]);
