/// <reference path="fourslash.ts" />

// @Filename: /a.ts
////export default function [|{| "isWriteAccess": true, "isDefinition": true |}f|]() {}

// @Filename: /b.ts
////import * as a from "./a";
////a.[|default|]();

const [r0, r1] = test.ranges();
verify.referenceGroups([r0, r1], [{ definition: "function f(): void", ranges: [r0, r1] }]);

verify.rangesAreRenameLocations([r0]);

goTo.rangeStart(r1);
verify.renameInfoFailed("");
