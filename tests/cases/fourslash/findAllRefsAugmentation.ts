/// <reference path='fourslash.ts' />
// @noLib: true

// @Filename: /node_modules/foo/index.d.ts
////export type [|{| "isWriteAccess": true, "isDefinition": true |}T|] = number;

// @Filename: /a.ts
////import * as foo from "foo";
////declare module "foo" {
////    export const x: [|T|];
////}

verify.singleReferenceGroup("type T = number");
