/// <reference path='fourslash.ts'/>

// @Filename: /defA.ts
////declare module "a" {
////    export type [|{| "isWriteAccess": true, "isDefinition": true |}T|] = number;
////}

// @Filename: /defB.ts
////declare module "b" {
////    export import a = require("a");
////    export const x: a.[|T|];
////}

// @Filename: /defC.ts
////declare module "c" {
////    import b = require("b");
////    const x: b.a.[|T|];
////}

//TODO: test more kinds of export-import
//TODO: this resembles referencesForAmbients, put it near there

verify.noErrors();
verify.singleReferenceGroup("type T = number");

