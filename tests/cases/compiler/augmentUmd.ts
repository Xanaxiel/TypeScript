// @Filename: /node_modules/foo/index.d.ts
export const x: number;
export as namespace Foo;

// @Filename: /node_modules/foo-ext/index.d.ts
import * as F from "foo";
declare module "foo" {
    export function f(): void;
}

// @Filename: /a.ts
Foo.f();
