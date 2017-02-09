/* Code for finding imports of an exported symbol. Used only by FindAllReferences. */
/* @internal */
namespace ts.FindAllReferences {
    export interface ImportsResult {
        /** A local search for every import of the symbol. */
        importSearches: Array<[Node, Symbol]>;
        /** Imports that may be added as references immediately, without further searching in the file. */
        singleReferences: Identifier[];
        /** List of source files that may (or may not) use the symbol via a namespace. */
        indirectUsers: SourceFile[];
    }
    export type ImportTracker = (exportSymbol: Symbol, exportInfo: ExportInfo, isForRename: boolean) => ImportsResult;

    export function createImportTracker(sourceFiles: SourceFile[], checker: TypeChecker): ImportTracker {
        const allDirectImports = getAllDirectImports(sourceFiles, checker);

        return (exportSymbol, exportInfo, isForRename) => {
            const { directImports, indirectUsers } = getImporters(exportInfo, checker);
            return { indirectUsers, ...getSearchesFromDirectImports(directImports, exportSymbol, exportInfo.exportKind, checker, isForRename) };
        }

        function getImporters({ exportingModuleSymbol, exportKind }: ExportInfo, checker: TypeChecker): { directImports: Importer[], indirectUsers: SourceFile[] } {
            const markSeenDirectImport = nodeSeenTracker<ImporterOrCallExpression>();
            const markSeenIndirectUser = nodeSeenTracker<ModuleDeclarationLike>();
            const directImports: Importer[] = [];
            const isGlobal = !!exportingModuleSymbol.globalExports;
            const indirectUserDeclarations: ModuleDeclarationLike[] = isGlobal ? undefined : []; //Only used if !isGlobal

            handleDirectImports(exportingModuleSymbol);

            return { directImports, indirectUsers: getIndirectUsers() }; //this may return duplicate indirect users. But we'll count on `markSearched`.

            function getIndirectUsers(): SourceFile[] {
                if (isGlobal) {
                    // It has `export as namespace`, so anything could potentially use it.
                    return sourceFiles;
                }

                // Module augmentations may use this module's exports without importing it.
                for (const decl of exportingModuleSymbol.declarations) {
                    if (ts.isExternalModuleAugmentation(decl)) {
                        addIndirectUser(decl as ModuleDeclarationLike);
                    }
                }

                // This may return duplicates (if there are multiple module declarations in a single source file, all importing the same thing as a namespace), but `State.markSearchedSymbol` will handle that.
                return indirectUserDeclarations.map(getSourceFileOfNode);
            }

            function handleDirectImports(exportingModuleSymbol: Symbol): void {
                const theseDirectImports = getDirectImports(exportingModuleSymbol);
                if (theseDirectImports) for (const direct of theseDirectImports) {
                    if (!markSeenDirectImport(direct)) {
                        continue;
                    }

                    switch (direct.kind) {
                        case SyntaxKind.CallExpression:
                            if (!isGlobal) {
                                // Don't support re-exporting 'require()' calls, so just add a single indirect user.
                                addIndirectUser(direct.getSourceFile());
                            }
                            break;

                        case SyntaxKind.ImportEqualsDeclaration:
                            handleNamespaceImport(direct, direct.name, hasModifier(direct, ModifierFlags.Export));
                            break;

                        case SyntaxKind.ImportDeclaration:
                            const namedBindings = direct.importClause && direct.importClause.namedBindings;
                            if (namedBindings && namedBindings.kind === SyntaxKind.NamespaceImport) {
                                handleNamespaceImport(direct, namedBindings.name)
                            } else {
                                directImports.push(direct);
                            }
                            break;

                        case SyntaxKind.ExportDeclaration:
                            //If `direct.exportClause`, this is `export { foo } from "foo"` and it creates an alias, so recursive search will get it.
                            if (!direct.exportClause) {
                                // This is `export * from "foo"`, so *this* module's direct imports need to be considered too!
                                handleDirectImports(getContainingModuleSymbol(direct, checker));
                            }
                            else { directImports.push(direct); }
                            break;
                    }
                }
            }

            function handleNamespaceImport(importDeclaration: ImportEqualsDeclaration | ImportDeclaration, name: Identifier, isReExport?: boolean): void {
                if (exportKind === ExportKind.ExportEquals) {
                    // Then this is a direct import, not import-as-namespace.
                    directImports.push(importDeclaration);
                }
                else if (!isGlobal) {
                    const sourceFileLike = getSourceFileLikeForImportDeclaration(importDeclaration);
                    Debug.assert(sourceFileLike.kind === SyntaxKind.SourceFile || sourceFileLike.kind === SyntaxKind.ModuleDeclaration);
                    if (isReExport || findNamespaceReExports(sourceFileLike, name, checker)) {
                        addIndirectUsers(sourceFileLike);
                    } else {
                        addIndirectUser(sourceFileLike);
                    }
                }
            }

            function addIndirectUser(sourceFileLike: ModuleDeclarationLike): boolean {
                Debug.assert(!isGlobal);
                const isNew = markSeenIndirectUser(sourceFileLike);
                if (isNew) {
                    indirectUserDeclarations.push(sourceFileLike);
                }
                return isNew;
            }

            /** Adds a module and all of its transitive dependencies as possible indirect users. */
            function addIndirectUsers(sourceFileLike: ModuleDeclarationLike): void {
                if (!addIndirectUser(sourceFileLike)) {
                    return;
                }

                const moduleSymbol = checker.getMergedSymbol(sourceFileLike.symbol);
                Debug.assert(!!(moduleSymbol.flags & SymbolFlags.Module));
                const directImports = getDirectImports(moduleSymbol);
                if (directImports) for (const directImport of directImports) {
                    addIndirectUsers(getSourceFileLikeForImportDeclaration(directImport));
                }
            }
        }

        function getDirectImports(moduleSymbol: Symbol): ImporterOrCallExpression[] | undefined {
            return allDirectImports[getSymbolId(moduleSymbol)];
        }
    }

    type ModuleDeclarationLike = SourceFile | ModuleDeclaration;
    type Importer = AnyImportSyntax | ExportDeclaration;
    type ImporterOrCallExpression = Importer | CallExpression;

    /**
     * Given the set of direct imports of a module, we need to find which ones import the particular exported symbol.
     * The returned `importSearches` will result in the entire source file being searched.
     * But re-exports will be placed in 'singleReferences' since they cannot be locally referenced.
     */
    function getSearchesFromDirectImports(directImports: Importer[], exportSymbol: Symbol, exportKind: ExportKind, checker: TypeChecker, isForRename: boolean): Pick<ImportsResult, "importSearches" | "singleReferences"> {
        const exportName = exportSymbol.name;
        const importSearches: Array<[Node, Symbol]> = [];
        let singleReferences: Identifier[] = []; //KILL!
        function addSearch(location: Node, symbol: Symbol) {
            //Since we're searching for imports, the comingFrom must be 'Export'. This means we won't recursively search for the exported ting for an import, since we've been there before.
            importSearches.push([location, symbol]);
        }

        if (directImports) for (const decl of directImports) {
            handleImportLike(decl);
        }

        return { importSearches, singleReferences };

        function handleImportLike(decl: Importer): void {
            //move
            function handleNamespaceImportLike(importName: Identifier) {
                if (isForRename && importName.text !== exportName) {
                    return;
                }

                addSearch(importName, checker.getSymbolAtLocation(importName));
            }

            if (decl.kind === SyntaxKind.ImportEqualsDeclaration) {
                const { name, moduleReference } = decl;
                if (exportKind === ExportKind.ExportEquals &&
                    moduleReference.kind === SyntaxKind.ExternalModuleReference &&
                    moduleReference.expression.kind === SyntaxKind.StringLiteral) {
                    handleNamespaceImportLike(name);
                }
                return;
            }

            const { moduleSpecifier } = decl;
            Debug.assert(moduleSpecifier.kind === SyntaxKind.StringLiteral);

            if (decl.kind === SyntaxKind.ExportDeclaration) {
                searchForNamedImport(decl.exportClause);
                return;
            }

            const { importClause } = decl;

            const { namedBindings } = importClause;
            if (namedBindings && namedBindings.kind === ts.SyntaxKind.NamespaceImport) {
                //`import * as x from "./x"
                //Text search will catch this (must use the name)
                //An `export =` may be imported by a namespace import.
                if (exportKind === ExportKind.ExportEquals) {
                    handleNamespaceImportLike(namedBindings.name);
                }

                return;
            }

            if (exportKind === ExportKind.Named) {
                searchForNamedImport(namedBindings as NamedImports | undefined);
            }
            else {
                // `export =` might be imported by a default import if `--allowSyntheticDefaultImports` is on, so this handles both ExportKind.Default and ExportKind.ExportEquals
                const { name } = importClause;
                // If a default import has the same name as the default export, allow to rename it.
                // Given `import f` and `export default function f`, we will rename both, but for `import g` we will rename just that.
                if (name && (!isForRename || name.text === symbolName(exportSymbol))) {
                    const defaultImportAlias = checker.getSymbolAtLocation(name);
                    addSearch(name, defaultImportAlias);
                }

                // 'default' might be accessed as a named import `{ default as foo }`.
                if (!isForRename && exportKind === ExportKind.Default) {
                    Debug.assert(exportName === "default");
                    searchForNamedImport(namedBindings as NamedImports | undefined);
                }
            }
        }

        function searchForNamedImport(namedBindings: NamedImportsOrExports | undefined): void {
            if (namedBindings) for (const element of namedBindings.elements) {
                const { name, propertyName } = element;
                if ((propertyName || name).text !== exportName) return;

                if (propertyName) {
                    //This is `import { foo as bar } from "./a"` or `export { foo as bar } from "./a"`. `foo` isn't a local in the file, so just add it as a single.
                    singleReferences.push(propertyName);
                    if (!isForRename) { //For a rename, don't continue looking past rename imports. In `import { foo as bar }`, don't touch `bar`, just `foo`.
                        addSearch(name, checker.getSymbolAtLocation(name));
                    }
                }
                else {
                    const localSymbol = element.kind === SyntaxKind.ExportSpecifier && element.propertyName
                        ? checker.getExportSpecifierLocalTargetSymbol(element)
                        : checker.getSymbolAtLocation(name);
                        //was: `element.kind === SyntaxKind.ExportSpecifier ? checker.getExportSpecifierLocalTargetSymbol(element) : checker.getSymbolAtLocation(name);`
                        //But if there's no propertyName, `getExportSpecifierLocalTargetSymbol` will return the symbol of the original declaration. Not the re-export.
                    addSearch(name, localSymbol);
                }
            }
        }
    }

    //Returns 'true' if the namespace is re-exported from this module.
    function findNamespaceReExports(sourceFileLike: ModuleDeclarationLike, name: Identifier, checker: TypeChecker): boolean {
        const namespaceImportSymbol = checker.getSymbolAtLocation(name);

        return forEachPossibleImportOrExportStatement(sourceFileLike, statement => {
            if (statement.kind !== SyntaxKind.ExportDeclaration) return;

            const { exportClause, moduleSpecifier } = statement as ExportDeclaration;
            if (moduleSpecifier || !exportClause) return;

            for (const element of exportClause.elements) {
                const x = checker.getExportSpecifierLocalTargetSymbol(element);
                if (x === namespaceImportSymbol) {
                    return true;
                }
            }
        });
    }

    //Returns a map from module symbol Id to import statements of that module.
    function getAllDirectImports(sourceFiles: SourceFile[], checker: TypeChecker): ImporterOrCallExpression[][] {
        const map: ImporterOrCallExpression[][] = [];

        for (const sourceFile of sourceFiles) {
            forEachImport(sourceFile, (importDecl, moduleSpecifier) => {
                const moduleSymbol = checker.getSymbolAtLocation(moduleSpecifier);
                if (moduleSymbol) {
                    const id = getSymbolId(moduleSymbol);
                    let imports = map[id];
                    if (!imports) {
                        imports = map[id] = [];
                    }
                    imports.push(importDecl);
                }
            });
        }

        return map;
    }


    function forEachPossibleImportOrExportStatement<T>(sourceFile: ModuleDeclarationLike, action: (statement: Statement) => T | undefined): T | undefined { //TODO: no T, just use void
        const statements = sourceFile.kind === SyntaxKind.SourceFile ? sourceFile.statements : (sourceFile.body as ModuleBlock).statements;
        for (const statement of statements) {
            const x = action(statement);
            if (x !== undefined) return x;

            if (statement.kind === SyntaxKind.ModuleDeclaration) {
                const decl = statement as ModuleDeclaration;
                if (decl.name.kind === SyntaxKind.StringLiteral) {
                    if (decl.body) for (const statement of (decl.body as ModuleBlock).statements) {
                        const x = action(statement);
                        if (x !== undefined) return x;
                    }
                }
            }
        }
    }

    //Also provides `require()` calls in JS
    function forEachImport(sourceFile: SourceFile, action: (importStatement: ImporterOrCallExpression, imported: StringLiteral) => void): void {
        if (sourceFile.externalModuleIndicator) {
            for (const moduleSpecifier of sourceFile.imports) {
                Debug.assert(moduleSpecifier.kind === SyntaxKind.StringLiteral);
                let importDecl = moduleSpecifier.parent;
                if (importDecl.kind !== SyntaxKind.ImportDeclaration && importDecl.kind !== SyntaxKind.ExportDeclaration) {
                    Debug.assert(importDecl.kind === SyntaxKind.ExternalModuleReference);
                    importDecl = importDecl.parent;
                    Debug.assert(importDecl.kind === SyntaxKind.ImportEqualsDeclaration);
                }
                action(importDecl as AnyImportSyntax | ExportDeclaration, moduleSpecifier as StringLiteral);
            }
        }
        else {
            forEachPossibleImportOrExportStatement(sourceFile, statement => {
                switch (statement.kind) {
                    case SyntaxKind.ExportDeclaration:
                    case SyntaxKind.ImportDeclaration: {
                        const decl = statement as ImportDeclaration | ExportDeclaration;
                        if (decl.moduleSpecifier) {
                            Debug.assert(decl.moduleSpecifier.kind === SyntaxKind.StringLiteral);
                            action(decl, decl.moduleSpecifier as StringLiteral);
                        }
                        break;
                    }

                    case SyntaxKind.ImportEqualsDeclaration: {
                        const decl = statement as ImportEqualsDeclaration;
                        const { moduleReference } = decl;
                        if (moduleReference.kind === SyntaxKind.ExternalModuleReference &&
                            moduleReference.expression.kind === SyntaxKind.StringLiteral) {
                            action(decl, moduleReference.expression as StringLiteral);
                        }
                        break;
                    }
                }
            });

            if (sourceFile.flags & NodeFlags.JavaScriptFile) {
                //Find all 'require()' calls.
                recur(sourceFile);
                function recur(node: Node) {
                    if (isRequireCall(node, true)) {
                        action(node, node.arguments[0] as StringLiteral);
                    }
                    forEachChild(node, recur);
                }
            }
        }
    }

    function forEachImportInNode(sourceFile: SourceFile | ModuleBlock, action: (importDeclaration: AnyImportSyntax | ExportDeclaration) => void): void {
        for (const statement of sourceFile.statements) {
            switch (statement.kind) {
                case SyntaxKind.ImportDeclaration:
                case SyntaxKind.ImportEqualsDeclaration:
                    action(statement as ImportDeclaration | ImportEqualsDeclaration);
                    break;

                case SyntaxKind.ExportDeclaration: {
                    const decl = statement as ExportDeclaration;
                    if (decl.moduleSpecifier) {
                        action(decl);
                    }
                    break;
                }

                case SyntaxKind.ModuleDeclaration: {
                    const decl = statement as ModuleDeclaration;
                    //No imports in namespaces, right???
                    if (decl.name.kind === SyntaxKind.StringLiteral) {
                        forEachImportInNode(decl.body as ModuleBlock, action);
                    }
                    break;
                }
            }
        }
    }

    //!
    function getExportNodeFromNodeNodeNode(parent: Node) {//name
        if (parent.kind === SyntaxKind.VariableDeclaration) {
            return getAncestor(parent, SyntaxKind.VariableStatement);
        }
        return parent;
    }

    /** Info about an exported symbol to perform recursive search on. */
    export interface ExportInfo {
        exportingModuleSymbol: Symbol;
        exportKind: ExportKind;
    }

    export const enum ImportExport { Import, Export }

    export interface ImportedSymbol {
        kind: ImportExport.Import;
        symbol: Symbol;
        isEqualsOrDefault: boolean;
    }
    export interface ExportedSymbol {
        kind: ImportExport.Export;
        symbol: Symbol;
        info: ExportInfo;
        shouldAddReference?: Identifier;
    }

    /**
     * Given a local reference, we might notice that it's an import/export and recursively search for references of that.
     * If at an import, look locally for the symbol it imports.
     * If an an export, look for all imports of it.
     */
    export function getImportExportSymbols(node: Node, symbol: Symbol, checker: TypeChecker, comingFromExport: boolean): ImportedSymbol | ExportedSymbol | undefined {
        const { parent } = node;

        function exportInfo(symbol: Symbol, kind: ExportKind, shouldAddReference?: Identifier): ExportedSymbol {
            const info = getExportInfo(symbol, kind, checker);
            return info && { kind: ImportExport.Export, symbol, info, shouldAddReference }
        }

        if (symbol.flags & SymbolFlags.Export) {
            if (symbol.declarations.some(d => d === parent)) {
                switch (getSpecialPropertyAssignmentKind(parent.parent)) {
                    case SpecialPropertyAssignmentKind.ExportsProperty:
                        return exportInfo(symbol, ExportKind.Named);
                    case SpecialPropertyAssignmentKind.ModuleExports:
                        //TODO:TEST
                        return exportInfo(symbol, ExportKind.ExportEquals);
                    case SpecialPropertyAssignmentKind.PrototypeProperty:
                    case SpecialPropertyAssignmentKind.ThisProperty:
                        return undefined;
                    case SpecialPropertyAssignmentKind.None:
                        //test
                        const { exportSymbol } = symbol;
                        Debug.assert(!!exportSymbol);
                        return exportInfo(exportSymbol, getExportKindForNode(parent));
                }
            }
        } else {
            const x = getExportNodeFromNodeNodeNode(parent);
            if (hasModifier(x, ModifierFlags.Export)) {
                let shouldAddReference: Identifier | undefined;
                if (x.kind === SyntaxKind.ImportEqualsDeclaration && (x as ImportEqualsDeclaration).moduleReference === node) {
                    //We're at `Y` in `export import X = Y`. This is not the exported symbol, the left-hand-side is.
                    //Then this is really an import.
                    const lhsSymbol = checker.getSymbolAtLocation((x as ImportEqualsDeclaration).name);
                    return { kind: ImportExport.Import, symbol: lhsSymbol, isEqualsOrDefault: true };
                }
                else
                    return exportInfo(symbol, getExportKindForNode(x), shouldAddReference);
            }
            else if (parent.kind === SyntaxKind.ExportAssignment) {
                // Get the symbol for the `export =` node; its parent is the module it's the export of.
                const exportingModuleSymbol = parent.symbol.parent;
                Debug.assert(!!exportingModuleSymbol);
                return { kind: ImportExport.Export, symbol, info: { exportingModuleSymbol, exportKind: ExportKind.ExportEquals } }
            }
        }

        if (comingFromExport) {
            return undefined;
        }

        const isImport = nodeIsImport(node);
        if (isImport) {
            // A symbol being imported is always an alias. So get what that aliases to find the local symbol.
            let importedSymbol = checker.getImmediateAliasedSymbol(symbol);
            if (importedSymbol) {
                // Want to search on the local symbol in the exporting module, not the exported symbol.
                importedSymbol = skipExportSpecifierSymbol(importedSymbol, checker);
                //Similarly, skip past the symbol for 'export ='
                if (importedSymbol.name === "export=") {
                    importedSymbol = checker.getImmediateAliasedSymbol(importedSymbol);
                }

                if (symbolName(importedSymbol) === symbol.name) { //If this is a rename import, do not continue searching.
                    return { kind: ImportExport.Import, symbol: importedSymbol, ...isImport };
                }
            }
        }

        return undefined;
    }

    function nodeIsImport(node: Node): { isEqualsOrDefault: boolean } | undefined {
        const { parent } = node;
        switch (parent.kind) {
            case SyntaxKind.ImportEqualsDeclaration:
                return (parent as ImportEqualsDeclaration).name === node ? { isEqualsOrDefault: true } : undefined;

            case SyntaxKind.ImportSpecifier: {
                //test!
                //If we're importing `{foo as bar}`, don't continue finding if there's a rename.
                //E.g. `import  {foo as bar }`
                //If we're at `bar`, don't continue looking for the imported symbol.
                //But if we're at `foo`, then do.
                const { propertyName } = parent as ImportSpecifier;
                return propertyName === undefined || propertyName === node ? { isEqualsOrDefault: false } : undefined;
            }

            case SyntaxKind.ImportClause:
                Debug.assert((parent as ImportClause).name === node);
                return { isEqualsOrDefault: true };

            case SyntaxKind.NamespaceImport:
                return { isEqualsOrDefault: true };

            default:
                return undefined;
        }
    }

    export function getExportInfo(exportSymbol: Symbol, exportKind: ExportKind, checker: TypeChecker): ExportInfo | undefined {
        const exportingModuleSymbol = checker.getMergedSymbol(exportSymbol.parent); // Need to get merged symbol in case there's an augmentation.
        // `export` may appear in a namespace. In that case, just rely on global search.
        return isExternalModuleSymbol(exportingModuleSymbol) ? { exportingModuleSymbol, exportKind } : undefined;
    }

    function symbolName(symbol: Symbol): string {
        if (symbol.name !== "default") {
            return symbol.name;
        }

        const name = forEach(symbol.declarations, d => {
            if (d.name && d.name.kind === SyntaxKind.Identifier) {
                return d.name.text;
            }
        });
        Debug.assert(!!name);
        return name;
    }

    export const enum ExportKind { Named, Default, ExportEquals }
    //Not meant for use with export specifiers or export assignment.
    function getExportKindForNode(node: Node): ExportKind | undefined {
        if (hasModifier(node, ModifierFlags.Default)) {
             return ExportKind.Default;
        }
        return ExportKind.Named;
    }

    //utils below

    /** If at an export specifier, go to the symbol it refers to. */
    function skipExportSpecifierSymbol(symbol: Symbol, checker: TypeChecker): Symbol {
        //isShorthandExportSpecifier could be a useful fn!
        if (symbol.declarations.some(node => node.kind === SyntaxKind.ExportSpecifier && !(node as ExportSpecifier).propertyName && !(node as ExportSpecifier).parent.parent.moduleSpecifier)) {
            return checker.getShallowTargetOfExportSpecifier(symbol);
        }
        return symbol;
    }

    function getContainingModuleSymbol(importer: Importer, checker: TypeChecker): Symbol { //name
        return checker.getMergedSymbol(getSourceFileLikeForImportDeclaration(importer).symbol);
    }

    //name
    function getSourceFileLikeForImportDeclaration(node: ImporterOrCallExpression): ModuleDeclarationLike {
        if (node.kind === SyntaxKind.CallExpression) {
            return node.getSourceFile();
        }

        const { parent } = node;

        if (parent.kind === SyntaxKind.SourceFile) {
            return parent as SourceFile;
        }
        Debug.assert(parent.kind === SyntaxKind.ModuleBlock && parent.parent.kind === SyntaxKind.ModuleDeclaration && (parent.parent as ModuleDeclaration).name.kind === SyntaxKind.StringLiteral); //helper
        return parent.parent as ModuleDeclaration;
    }
}
