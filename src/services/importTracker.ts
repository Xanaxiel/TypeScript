/* Code for finding imports of an exported symbol. Used only by FindAllReferences. */
/* @internal */
namespace ts.FindAllReferences {
    export interface ImportsResult {
        /** A local search for every import of the symbol. */
        importSearches: Array<[Identifier, Symbol]>;
        /** Imports that may be added as references immediately, without further searching in the file. */
        singleReferences: Identifier[];
        /** List of source files that may (or may not) use the symbol via a namespace. */
        indirectUsers: SourceFile[];
    }
    export type ImportTracker = (exportSymbol: Symbol, exportInfo: ExportInfo, isForRename: boolean) => ImportsResult;

    export function createImportTracker(sourceFiles: SourceFile[], checker: TypeChecker): ImportTracker {
        const allDirectImports = getDirectImportsMap(sourceFiles, checker);
        return (exportSymbol, exportInfo, isForRename) => {
            const { directImports, indirectUsers } = getImportersForExport(sourceFiles, allDirectImports, exportInfo, checker);
            return { indirectUsers, ...getSearchesFromDirectImports(directImports, exportSymbol, exportInfo.exportKind, checker, isForRename) };
        }
    }

    /** Info about an exported symbol to perform recursive search on. */
    export interface ExportInfo {
        exportingModuleSymbol: Symbol;
        exportKind: ExportKind;
    }

    export const enum ExportKind { Named, Default, ExportEquals } //move

    export const enum ImportExport { Import, Export }

    interface AmbientModuleDeclaration extends ModuleDeclaration { body?: ModuleBlock; }
    type SourceFileLike = SourceFile | AmbientModuleDeclaration;
    type Importer = AnyImportSyntax | ExportDeclaration;
    type ImporterOrCallExpression = Importer | CallExpression;

    /** Returns import statements that directly reference the exporting module, and a list of files that may access the module through a namespace. */
    function getImportersForExport(sourceFiles: SourceFile[], allDirectImports: ImporterOrCallExpression[][], { exportingModuleSymbol, exportKind }: ExportInfo, checker: TypeChecker): { directImports: Importer[], indirectUsers: SourceFile[] } {
        const markSeenDirectImport = nodeSeenTracker<ImporterOrCallExpression>();
        const markSeenIndirectUser = nodeSeenTracker<SourceFileLike>();
        const directImports: Importer[] = [];
        const isAvailableThroughGlobal = !!exportingModuleSymbol.globalExports;
        const indirectUserDeclarations: SourceFileLike[] = isAvailableThroughGlobal ? undefined : [];

        handleDirectImports(exportingModuleSymbol);

        return { directImports, indirectUsers: getIndirectUsers() };

        function getIndirectUsers(): SourceFile[] {
            if (isAvailableThroughGlobal) {
                // It has `export as namespace`, so anything could potentially use it.
                return sourceFiles;
            }

            // Module augmentations may use this module's exports without importing it.
            for (const decl of exportingModuleSymbol.declarations) {
                if (ts.isExternalModuleAugmentation(decl)) {
                    addIndirectUser(decl as SourceFileLike);
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
                        if (!isAvailableThroughGlobal) {
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
                        if (!direct.exportClause) {
                            // This is `export * from "foo"`, so imports of this module may import the export too.
                            handleDirectImports(getContainingModuleSymbol(direct, checker));
                        }
                        else {
                            // This is `export { foo } from "foo"` and creates an alias symbol, so recursive search will get handle re-exports.
                            directImports.push(direct);
                        }
                        break;
                }
            }
        }

        function handleNamespaceImport(importDeclaration: ImportEqualsDeclaration | ImportDeclaration, name: Identifier, isReExport?: boolean): void {
            if (exportKind === ExportKind.ExportEquals) {
                // This is a direct import, not import-as-namespace.
                directImports.push(importDeclaration);
            }
            else if (!isAvailableThroughGlobal) {
                const sourceFileLike = getSourceFileLikeForImportDeclaration(importDeclaration);
                Debug.assert(sourceFileLike.kind === SyntaxKind.SourceFile || sourceFileLike.kind === SyntaxKind.ModuleDeclaration);
                if (isReExport || findNamespaceReExports(sourceFileLike, name, checker)) {
                    addIndirectUsers(sourceFileLike);
                } else {
                    addIndirectUser(sourceFileLike);
                }
            }
        }

        function addIndirectUser(sourceFileLike: SourceFileLike): boolean {
            Debug.assert(!isAvailableThroughGlobal);
            const isNew = markSeenIndirectUser(sourceFileLike);
            if (isNew) {
                indirectUserDeclarations.push(sourceFileLike);
            }
            return isNew;
        }

        /** Adds a module and all of its transitive dependencies as possible indirect users. */
        function addIndirectUsers(sourceFileLike: SourceFileLike): void {
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

        function getDirectImports(moduleSymbol: Symbol): ImporterOrCallExpression[] | undefined {
            return allDirectImports[getSymbolId(moduleSymbol)];
        }
    }

    /**
     * Given the set of direct imports of a module, we need to find which ones import the particular exported symbol.
     * The returned `importSearches` will result in the entire source file being searched.
     * But re-exports will be placed in 'singleReferences' since they cannot be locally referenced.
     */
    function getSearchesFromDirectImports(directImports: Importer[], exportSymbol: Symbol, exportKind: ExportKind, checker: TypeChecker, isForRename: boolean): Pick<ImportsResult, "importSearches" | "singleReferences"> {
        const exportName = exportSymbol.name;
        const importSearches: Array<[Identifier, Symbol]> = [];
        let singleReferences: Identifier[] = []; //KILL!
        function addSearch(location: Identifier, symbol: Symbol) {
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
            // Ignore if there's a grammar error
            if (moduleSpecifier.kind !== SyntaxKind.StringLiteral) return;

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

    /** Returns 'true' is the namespace 'name' is re-exported from this module, and 'false' if it is only used locally. */
    function findNamespaceReExports(sourceFileLike: SourceFileLike, name: Identifier, checker: TypeChecker): boolean {
        const namespaceImportSymbol = checker.getSymbolAtLocation(name);

        return forEachPossibleImportOrExportStatement(sourceFileLike, statement => {
            if (statement.kind !== SyntaxKind.ExportDeclaration) return;

            const { exportClause, moduleSpecifier } = statement as ExportDeclaration;
            if (moduleSpecifier || !exportClause) return;

            for (const element of exportClause.elements) {
                if (checker.getExportSpecifierLocalTargetSymbol(element) === namespaceImportSymbol) {
                    return true;
                }
            }
        });
    }

    /** Returns a map from a module symbol Id to all import statements that directly reference the module. */
    function getDirectImportsMap(sourceFiles: SourceFile[], checker: TypeChecker): ImporterOrCallExpression[][] {
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

    /** Iterates over all statements at the top level or in module declarations. Returns the first truthy result. */
    function forEachPossibleImportOrExportStatement<T>(sourceFileLike: SourceFileLike, action: (statement: Statement) => T): T {
        return forEach(sourceFileLike.kind === SyntaxKind.SourceFile ? sourceFileLike.statements : sourceFileLike.body.statements, statement =>
            action(statement) || (isAmbientModuleDeclaration(statement) && forEach(statement.body && statement.body.statements, action)));
    }

    /** Calls `action` for each import, re-export, or require() in a file. */
    function forEachImport(sourceFile: SourceFile, action: (importStatement: ImporterOrCallExpression, imported: StringLiteral) => void): void {
        if (sourceFile.externalModuleIndicator) {
            for (const moduleSpecifier of sourceFile.imports) {
                action(importerFromModuleSpecifier(moduleSpecifier), moduleSpecifier);
            }
        }
        else {
            forEachPossibleImportOrExportStatement(sourceFile, statement => {
                switch (statement.kind) {
                    case SyntaxKind.ExportDeclaration:
                    case SyntaxKind.ImportDeclaration: {
                        const decl = statement as ImportDeclaration | ExportDeclaration;
                        if (decl.moduleSpecifier && decl.moduleSpecifier.kind === SyntaxKind.StringLiteral) {
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
                // Find all 'require()' calls.
                recur(sourceFile);
                function recur(node: Node) {
                    if (isRequireCall(node, /*checkArgumentIsStringLiteral*/true)) {
                        action(node, node.arguments[0] as StringLiteral);
                    }
                    forEachChild(node, recur);
                }
            }
        }
    }

    function importerFromModuleSpecifier(moduleSpecifier: StringLiteral): Importer {
        const decl = moduleSpecifier.parent;
        if (decl.kind === SyntaxKind.ImportDeclaration || decl.kind === SyntaxKind.ExportDeclaration) {
            return decl as ImportDeclaration | ExportDeclaration;
        }
        Debug.assert(decl.kind === SyntaxKind.ExternalModuleReference);
        return (decl as ExternalModuleReference).parent;
    }

    export interface ImportedSymbol {
        kind: ImportExport.Import;
        symbol: Symbol;
        isNamedImport: boolean;
    }
    export interface ExportedSymbol {
        kind: ImportExport.Export;
        symbol: Symbol;
        info: ExportInfo;
    }
    /**
     * Given a local reference, we might notice that it's an import/export and recursively search for references of that.
     * If at an import, look locally for the symbol it imports.
     * If an an export, look for all imports of it.
     * @param comingFromExport If we are doing a search for all exports, don't bother looking backwards for the imported symbol, since that's the reason we're here.
     */
    export function getImportOrExportSymbol(node: Node, symbol: Symbol, checker: TypeChecker, comingFromExport: boolean): ImportedSymbol | ExportedSymbol | undefined {
        const ex = getExport();
        return ex || comingFromExport ? ex : getImport();

        function getExport(): ExportedSymbol | ImportedSymbol | undefined {
            const { parent } = node;
            if (symbol.flags & SymbolFlags.Export) {
                if (parent.kind === SyntaxKind.PropertyAccessExpression) {
                    // When accessing an export of a JS module, there's no alias. The symbol will still be flagged as an export even though we're at the use.
                    // So check that we are at the declaration.
                    if (!symbol.declarations.some(d => d === parent)) {
                        return undefined;
                    }

                    switch (getSpecialPropertyAssignmentKind(parent.parent)) {
                        case SpecialPropertyAssignmentKind.ExportsProperty:
                            return exportInfo(symbol, ExportKind.Named);
                        case SpecialPropertyAssignmentKind.ModuleExports:
                            return exportInfo(symbol, ExportKind.ExportEquals);
                        default:
                            return undefined;
                    }
                }
                else {
                    const { exportSymbol } = symbol;
                    Debug.assert(!!exportSymbol);
                    return exportInfo(exportSymbol, getExportKindForDeclaration(parent));
                }
            } else {
                const exportNode = parent.kind === SyntaxKind.VariableDeclaration ? getAncestor(parent, SyntaxKind.VariableStatement) : parent;
                if (hasModifier(exportNode, ModifierFlags.Export)) {
                    if (exportNode.kind === SyntaxKind.ImportEqualsDeclaration && (exportNode as ImportEqualsDeclaration).moduleReference === node) {
                        //We're at `Y` in `export import X = Y`. This is not the exported symbol, the left-hand-side is.
                        //Then this is really an import.
                        if (comingFromExport) return undefined;
                        const lhsSymbol = checker.getSymbolAtLocation((exportNode as ImportEqualsDeclaration).name);
                        return { kind: ImportExport.Import, symbol: lhsSymbol, isNamedImport: false };
                    }
                    else {
                        return exportInfo(symbol, getExportKindForDeclaration(exportNode));
                    }
                }
                else if (parent.kind === SyntaxKind.ExportAssignment) {
                    // Get the symbol for the `export =` node; its parent is the module it's the export of.
                    const exportingModuleSymbol = parent.symbol.parent;
                    Debug.assert(!!exportingModuleSymbol);
                    return { kind: ImportExport.Export, symbol, info: { exportingModuleSymbol, exportKind: ExportKind.ExportEquals } }
                }
            }
        }

        function getImport(): ImportedSymbol | undefined {
            const isImport = isNodeImport(node);
            if (!isImport) return;

            // A symbol being imported is always an alias. So get what that aliases to find the local symbol.
            let importedSymbol = checker.getImmediateAliasedSymbol(symbol);
            if (importedSymbol) {
                // Search on the local symbol in the exporting module, not the exported symbol.
                importedSymbol = skipExportSpecifierSymbol(importedSymbol, checker);
                // Similarly, skip past the symbol for 'export ='
                if (importedSymbol.name === "export=") {
                    importedSymbol = checker.getImmediateAliasedSymbol(importedSymbol);
                }

                if (symbolName(importedSymbol) === symbol.name) { // If this is a rename import, do not continue searching.
                    return { kind: ImportExport.Import, symbol: importedSymbol, ...isImport };
                }
            }
        }

        function exportInfo(symbol: Symbol, kind: ExportKind): ExportedSymbol {
            const info = getExportInfo(symbol, kind, checker);
            return info && { kind: ImportExport.Export, symbol, info }
        }

        // Not meant for use with export specifiers or export assignment.
        function getExportKindForDeclaration(node: Node): ExportKind | undefined {
            return hasModifier(node, ModifierFlags.Default) ? ExportKind.Default : ExportKind.Named;
        }
    }

    function isNodeImport(node: Node): { isNamedImport: boolean } | undefined {
        const { parent } = node;
        switch (parent.kind) {
            case SyntaxKind.ImportEqualsDeclaration:
                return (parent as ImportEqualsDeclaration).name === node ? { isNamedImport: false } : undefined;
            case SyntaxKind.ImportSpecifier:
                // For a rename import `{ foo as bar }`, don't search for the imported symbol. Just find local uses of `bar`.
                return (parent as ImportSpecifier).propertyName ? undefined : { isNamedImport: true };
            case SyntaxKind.ImportClause:
            case SyntaxKind.NamespaceImport:
                Debug.assert((parent as ImportClause | NamespaceImport).name === node);
                return { isNamedImport: false };
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

        const name = forEach(symbol.declarations, ({ name }) => name && name.kind === SyntaxKind.Identifier && name.text);
        Debug.assert(!!name);
        return name;
    }

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

    function getSourceFileLikeForImportDeclaration(node: ImporterOrCallExpression): SourceFileLike {
        if (node.kind === SyntaxKind.CallExpression) {
            return node.getSourceFile();
        }

        const { parent } = node;

        if (parent.kind === SyntaxKind.SourceFile) {
            return parent as SourceFile;
        }
        Debug.assert(parent.kind === SyntaxKind.ModuleBlock && isAmbientModuleDeclaration(parent.parent));
        return parent.parent as AmbientModuleDeclaration;
    }

    function isAmbientModuleDeclaration(node: Node): node is AmbientModuleDeclaration {
        return node.kind === SyntaxKind.ModuleDeclaration && (node as ModuleDeclaration).name.kind === SyntaxKind.StringLiteral;
    }
}
