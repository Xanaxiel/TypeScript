/// <reference path="fourslash.ts" />

// @Filename: /a.ts
////export function [|{| "isWriteAccess": true, "isDefinition": true |}foo|](): void {}

// @Filename: /b.ts
////export { [|{| "isWriteAccess": true, "isDefinition": true |}foo|] } from "./a";

// @Filename: /c.ts
////export { [|{| "isWriteAccess": true, "isDefinition": true |}foo|] } from "./b";

// @Filename: /d.ts
////export { [|{| "isWriteAccess": true, "isDefinition": true |}foo|] } from "./c";

// @Filename: /e.ts
////import { [|{| "isWriteAccess": true, "isDefinition": true |}foo|] } from "./d";
////[|foo|]();

declare namespace ts {
    namespace performance {
        function getDuration(measureName: string): number;
        export function forEachMeasure(cb: (measureName: string, duration: number) => void): void;
        export function enable(): void;
    }
}

ts.performance.enable();

verify.noErrors();

const ranges = test.ranges();
const [a, b, c, d, e0, e1] = ranges;
verify.referenceGroups(a, [
    { definition: "function foo(): void", ranges: [a] },
    { definition: "import foo", ranges: [b] },
    { definition: "import foo", ranges: [c] },
    { definition: "import foo", ranges: [d] },
    { definition: "import foo", ranges: [e0, e1] }
]);

const start = process.hrtime();
for (let i = 0; i < 1000; i++) {
    test.timeFindAllRefs(1);
    test.cleanupSemanticCache();
}
const [durationSeconds, durationNanos] = process.hrtime(start);
const milliseconds = durationSeconds * 1000 + durationNanos / 1000000

ts.performance.forEachMeasure((name, duration) => {
    if (name === "Parse" || name === "Program" || name === "Bind" || name === "Check") return;
    console.log(`${name}: ${duration}`);
});

console.log(`Total: ${milliseconds} microseconds`);

