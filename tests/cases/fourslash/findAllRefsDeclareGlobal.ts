/// <reference path='fourslash.ts' />

// @noLib: true

// @Filename: /a.ts
////export {};
////declare global {
////    function [|{| "isWriteAccess": true, "isDefinition": true |}f|](): void;
////}

// @Filename: /b.ts
////[|f|]();

verify.noErrors();
verify.singleReferenceGroup("function f(): void");
