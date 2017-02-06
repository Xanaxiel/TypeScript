/// <reference path='fourslash.ts' />

// @noLib: true

// @Filename: /a.ts
////export default function [|{| "isWriteAccess": true, "isDefinition": true |}f|]() {}

// @Filename: /b.ts
////import * as a from "./a";
////a.[|default|]();

verify.singleReferenceGroup("function f(): void");
