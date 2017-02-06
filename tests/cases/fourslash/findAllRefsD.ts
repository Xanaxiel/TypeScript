/// <reference path='fourslash.ts' />

// @noLib: true

// @Filename: /a.ts
////export default function [|{| "isWriteAccess": true, "isDefinition": true |}f|]() {}

// @Filename: /b.ts
////export import a = require("./a");

// @Filename: /c.ts
////import { a } from "./b";
////a.[|default|]();

verify.singleReferenceGroup("function f(): void");
