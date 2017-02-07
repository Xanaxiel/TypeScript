/// <reference path="fourslash.ts" />

// @Filename: /a.ts
////export default function [|{| "isWriteAccess": true, "isDefinition": true |}f|](): void {}

// @Filename: /b.ts
////import * as a from "./a";
////a.[|default|]();

verify.singleReferenceGroup("function f(): void");
