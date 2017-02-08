/// <reference path="fourslash.ts" />

// @noLib: true

// @Filename: /a.ts
////export function [|{| "isWriteAccess": true, "isDefinition": true |}f|](): void {}

// @Filename: /b.ts
////import * as a from "./a";
////export { a };

// @Filename: /c.ts
////import * as b from "./b";
////b.a.[|f|]();

verify.noErrors();

const ranges = test.ranges();
const [r0, r1] = ranges;
verify.referenceGroups(r0, []);
