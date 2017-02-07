/// <reference path="fourslash.ts" />

// @Filename: /a.ts
////export default function [|{| "isWriteAccess": true, "isDefinition": true |}f|]() {}

// @Filename: /b.ts
////import * as a from "./a";
////a.[|default|]();

const ranges = test.ranges();
const [r0, r1] = ranges;
//verify.referenceGroups([r0, r1], [{ definition: "function f(): void", ranges: [r0, r1] }]);

verify.rangesAreRenameLocations(false, false, [r0]);

// TODO: Shouldn't be able to rename a default import.
// goTo.rangeStart(r1);
// verify.renameInfoFailed("");
