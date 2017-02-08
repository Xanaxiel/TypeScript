/// <reference path='fourslash.ts' />
// @noLib: true

////declare module "a" {
////    export class [|{| "isWriteAccess": true, "isDefinition": true |}C|] {}
////}
////declare module "b" {
////    export { [|C|] as [|{| "isWriteAccess": true, "isDefinition": true |}D|] } from "a";
////}
////declare module "c" {
////    import { [|{| "isWriteAccess": true, "isDefinition": true |}D|] } from "b";
////    export function f(c: [|D|]): void;
////}

verify.noErrors();

const ranges = test.rangesByText();
const cs = ranges.get("C");
const [c0, c1] = cs;
const [d0, d1, d2] = ranges.get("D");

verify.rangesWithSameTextAreRenameLocations();

const classes = { definition: "class C", ranges: [c0] };
const bImports = { definition: "import D", ranges: [d0] };
const cImports = { definition: "import D", ranges: [d1, d2] };
verify.referenceGroups(cs, [classes, bImports, cImports]);

verify.referenceGroups(d0, [bImports, cImports]);
verify.referenceGroups([d1, d2], [cImports, bImports]);

