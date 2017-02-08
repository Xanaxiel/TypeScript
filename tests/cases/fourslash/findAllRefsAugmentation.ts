/// <reference path='fourslash.ts' />
// @noLib: true

// @Filename: /node_modules/foo/index.d.ts
////export type [|{| "isWriteAccess": true, "isDefinition": true |}T|] = number;

// @Filename: /a.ts
////import * as foo from "foo";
////declare module "foo" {
////    export const x: [|T|];
////}

goTo.file("/a.ts");
verify.noErrors();
const [r0, r1] = test.ranges();
verify.referenceGroups(r0, [{ definition: "type T = number", ranges: [r0, r1] }]);
//verify.singleReferenceGroup("type T = number");
