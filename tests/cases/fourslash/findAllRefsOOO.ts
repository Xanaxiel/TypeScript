// @Filename: /a.ts
////export function [|{| "isWriteAccess": true, "isDefinition": true |}foo|](): void {}

// @Filename: /b.ts
////export { [|foo|] as [|{| "isWriteAccess": true, "isDefinition": true |}bar|] } from "./a";

// @Filename: /c.ts
////export { [|foo|] as [|{| "isWriteAccess": true, "isDefinition": true |}default|] } from "./a";

// @Filename: /d.ts
////import { [|{| "isWriteAccess": true, "isDefinition": true |}bar|] } from "./b";
////import [|{| "isWriteAccess": true, "isDefinition": true |}baz|] from "./c";
////import { [|default|] as [|{| "isWriteAccess": true, "isDefinition": true |}bang|] } from "./c";
////[|bar|](); [|baz|](); [|bang|]();

verify.noErrors();
const ranges = test.ranges();
const [foo0, foo1, bar0, foo2, default0, bar1, baz0, default1, bang0, bar2, baz1, bang1] = ranges;
const a = { definition: "function foo(): void", ranges: [foo0] };
const b = { definition: "import bar", ranges: [bar0] };
const c = { definition: "import default", ranges: [default0] };
const dBar = { definition: "import bar", ranges: [bar1, bar2] };
const dBaz = { definition: "import baz", ranges: [baz0, baz1] };
const dBang = { definition: "import bang", ranges: [bang0, bang1] };

verify.referenceGroups([foo0, foo1, foo2], [a, b, dBar, c, dBaz, dBang]);

verify.referenceGroups(bar0, [b, dBar]);
verify.referenceGroups(bar1, [dBar, b]);
verify.referenceGroups(bar2, [{ ...dBar, definition: "(alias) bar(): void\nimport bar" }, b]);

verify.referenceGroups([default0, default1], [c, dBaz, dBang]);
verify.referenceGroups(baz0, [dBaz]);
verify.referenceGroups(baz1, [{ ...dBaz, definition: "(alias) baz(): void\nimport baz" }]);

verify.referenceGroups(bang0, [dBang]);
verify.referenceGroups(bang1, [{ ...dBang, definition: "(alias) bang(): void\nimport bang" }]);
