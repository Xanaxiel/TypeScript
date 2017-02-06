/// <reference path='fourslash.ts'/>

// @Filename: /defA.ts
////declare module "a" {
////    export type [|{| "isWriteAccess": true, "isDefinition": true |}T|] = number;
////}

// @Filename: /defB.ts
////declare module "b" {
////    export import {| "isWriteAccess": true, "isDefinition": true |}a = require("a");
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

const ranges = test.ranges();
const [r0, r1, r2] = ranges;
//verify.singleReferenceGroup('module "foo"', [moduleFoo0, moduleFoo1]);
//verify.singleReferenceGroup('module "bar"', [moduleBar0, moduleBar1]);
//verify.singleReferenceGroup('import foo = require("foo")', [foo0, foo1, foo2]);
verify.referenceGroups(r0, [{ definition: "type T = number", ranges: [r0, r1, r2] }]);
//verify.singleReferenceGroup("var f: number", [f0, f1]);
