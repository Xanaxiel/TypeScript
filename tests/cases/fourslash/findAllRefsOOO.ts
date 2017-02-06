// @Filename: /a.ts
////export function [|{| "isWriteAccess": true, "isDefinition": true |}foo|](): void {}

// @Filename: /b.ts
////export { [|foo|] as [|{| "isWriteAccess": true, "isDefinition": true |}bar|] } from "./a";

// @Filename: /c.ts
////import { [|{| "isWriteAccess": true, "isDefinition": true |}bar|] } from "./b";

verify.noErrors();
const ranges = test.ranges();
const [r0, foo, r1, r2] = ranges;
const a = { definition: "function foo(): void", ranges: [r0] };
const b = { definition: "import bar", ranges: [r1] };
const c = { definition: "import bar", ranges: [r2] };
verify.referenceGroups([r0, foo], [a, b, c]);
verify.referenceGroups(r1, [b, c]);
verify.referenceGroups(r2, [c]);
