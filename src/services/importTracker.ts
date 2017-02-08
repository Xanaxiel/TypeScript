/* Code for finding imports of an exported symbol. Used only by FindAllReferences. */

/* @internal */
namespace ts.FindAllReferences {
    type SourceFileLike = SourceFile | ModuleDeclaration;
    type Importer = AnyImportSyntax | ExportDeclaration;
    type ImportLike2 = Importer | CallExpression;

    //neater
    function getModuleDeclaration(node: Node): SourceFileLike {
        //getAncestor
        while (true) {
            switch (node.kind) {
                case SyntaxKind.SourceFile:
                    return node as SourceFile;
                case SyntaxKind.ModuleDeclaration:
                    if ((node as ModuleDeclaration).name.kind === SyntaxKind.StringLiteral) {
                        return node as ModuleDeclaration;
                    }
            }
            node = node.parent;
        }
    }

    //move
    function getContainingModuleSymbol(direct: Importer, checker: TypeChecker): Symbol {
        return checker.getMergedSymbol(getModuleDeclaration(direct).symbol);
    }

    export interface ImportsResult {
        /** A local search for every import of the symbol. */
        importSearches: Search[];
        /** Imports that may be added as references immediately, without further searching in the file. */
        singleReferences: Node[];
        /** List of source files that may (or may not) use the symbol via a namespace. */
        indirectUsers: SourceFile[];
    }
    export type ImportTracker = (exportSymbol: Symbol, exportInfo: ExportInfo, state: State) => ImportsResult;

    export function createImportTracker(sourceFiles: SourceFile[], checker: TypeChecker): ImportTracker {
        const allDirectImports = getAllDirectImports(sourceFiles, checker);

        return (exportSymbol, exportInfo, state) => {
            const { directImports, indirectUsers } = getImporters(exportInfo, checker);
            return { indirectUsers, ...getSearchesFromDirectImports(exportSymbol, exportInfo, state, directImports) };
        }

        //Files that directly access the export.
        //Files that may (or may not) access the export through a namespace.
        function getImporters({ exportingModuleSymbol, exportKind }: ExportInfo, checker: TypeChecker): { directImports: Importer[], indirectUsers: SourceFile[] } {
            const seenDirectImports: Array<true> = [];
            const seenIndirectUsers: Array<true> = [];
            const indirectUsers: SourceFileLike[] = [];
            const directImports: Importer[] = [];

            handleDirectImports(getDirectImports(exportingModuleSymbol));

            //Augmentations of the module can also use its exports without needing an import statement.
            for (const decl of exportingModuleSymbol.declarations) {
                //Don't need to add the original.
                if (decl.kind !== SyntaxKind.SourceFile) {
                    Debug.assert(decl.kind === SyntaxKind.ModuleDeclaration);
                    addIndirectUser(decl as SourceFileLike);
                }
            }

            return { directImports, indirectUsers: indirectUsers.map(x => x.getSourceFile()) }; //this may return duplicate indirect users. But we'll count on `markSearched`.

            function handleDirectImports(theseDirectImports: ImportLike2[]): void {
                if (theseDirectImports) for (const direct of theseDirectImports) {
                    if (!markSeenDirectImport(direct)) continue;

                    switch (direct.kind) {
                        case SyntaxKind.CallExpression:
                            //Don't support re-exporting 'require()' calls
                            addIndirectUser(direct.getSourceFile());
                            break;

                        case SyntaxKind.ImportEqualsDeclaration:
                            handleNamespaceImportLike(direct, direct.name, hasModifier(direct, ModifierFlags.Export));
                            break;

                        case SyntaxKind.ImportDeclaration:
                            const namedBindings = direct.importClause && direct.importClause.namedBindings;
                            if (namedBindings && namedBindings.kind === SyntaxKind.NamespaceImport) {
                                handleNamespaceImportLike(direct, namedBindings.name)
                            } else {
                                directImports.push(direct);
                            }
                            break;

                        case SyntaxKind.ExportDeclaration:
                            //If `direct.exportClause`, this is `export { foo } from "foo"` and it creates an alias, so recursive search will get it.
                            if (!direct.exportClause) {
                                // This is `export * from "foo"`, so *this* module's direct imports need to be considered too!
                                handleDirectImports(getDirectImports(getContainingModuleSymbol(direct, checker)));
                            }
                            directImports.push(direct);
                            break;
                    }
                }
            }

            function markSeenDirectImport(direct: ImportLike2): boolean {
                const id = getNodeId(direct);
                return !seenDirectImports[id] && (seenDirectImports[id] = true);
            }

            //name
            function handleNamespaceImportLike(direct: ImportEqualsDeclaration | ImportDeclaration, name: Identifier, isReExport?: boolean) {
                if (exportKind === ExportKind.ExportEquals) {
                    //Then this is actually a direct import.
                    directImports.push(direct);
                }
                else {
                    handleNamespaceImport(direct, name, isReExport);
                }
            }

            function addIndirectUser(sourceFileLike: SourceFileLike): boolean {
                const id = getNodeId(sourceFileLike);
                const isNew = !seenIndirectUsers[id] && (seenIndirectUsers[id] = true);
                if (isNew) {
                    indirectUsers.push(sourceFileLike);
                }
                return isNew;
            }

            function handleNamespaceImport(importDeclaration: ImportDeclaration | ImportEqualsDeclaration, name: Identifier, isReExport?: boolean) {
                const sourceFileLike = getSourceFileLikeForImportDeclaration(importDeclaration);
                Debug.assert(sourceFileLike.kind === SyntaxKind.SourceFile || sourceFileLike.kind === SyntaxKind.ModuleDeclaration);
                if (isReExport || findNamespaceReExports(sourceFileLike, name, checker)) {
                    addIndirectUsers(sourceFileLike);
                } else {
                    addIndirectUser(sourceFileLike);
                }
            }

            function addIndirectUsers(sourceFileLike: SourceFileLike) {
                if (!addIndirectUser(sourceFileLike)) return;
                const moduleSymbol = sourceFileLike.symbol; //TODO: get merged symbol
                Debug.assert(!!(moduleSymbol.flags & SymbolFlags.Module));
                const direct = getDirectImports(moduleSymbol);
                if (direct) for (const d of direct) {
                    addIndirectUsers(getModuleDeclaration(d));
                }
            }
        }

        function getDirectImports(moduleSymbol: Symbol): ImportLike2[] | undefined {
            return allDirectImports[getSymbolId(moduleSymbol)];
        }
    }

    function getSearchesFromDirectImports(exportSymbol: Symbol, { exportKind }: ExportInfo, { createSearch, checker, isForRename }: State, directImports: Importer[]): { importSearches: Search[], singleReferences: Node[] } {
        const exportName = exportSymbol.name;
        const importSearches: Search[] = [];
        let singleReferences: Node[] = [];
        function addSearch(location: Node, symbol: Symbol) {
            //Since we're searching for imports, the comingFrom must be 'Export'. This means we won't recursively search for the exported ting for an import, since we've been there before.
            importSearches.push(createSearch(location, symbol, ImportExport.Export));
        }

        if (directImports) for (const decl of directImports) {
            handleImportLike(decl);
        }

        return { importSearches, singleReferences };

        function handleImportLike(decl: Importer): void {
            if (decl.kind === SyntaxKind.ImportEqualsDeclaration) {
                const { name, moduleReference } = decl;
                if (exportKind === ExportKind.ExportEquals &&
                    moduleReference.kind === SyntaxKind.ExternalModuleReference &&
                    moduleReference.expression.kind === SyntaxKind.StringLiteral) {

                    if (!isForRename || decl.name.text === exportSymbol.name) {
                        addSearch(name, checker.getSymbolAtLocation(name));
                    }
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
                    //Don't need to check for match with exportSymbol since we've checked the module. (In fact, the imported symbol will be different.)
                    const location = namedBindings.name;
                    addSearch(location, checker.getSymbolAtLocation(location)); //duplicate code
                }

                return;
            }

            if (exportKind === ExportKind.Named) {
                searchForNamedImport(namedBindings as NamedImports | undefined);
            }
            else {
                //`export =` might be imported by a default import if `--allowSyntheticDefaultExports` is on, so this handles both ExportKind.Default and ExportKind.ExportEquals
                const { name } = importClause;
                if (name && (!isForRename || name.text === symbolName(exportSymbol))) {
                    const defaultImportAlias = checker.getSymbolAtLocation(name);
                    Debug.assert(checker.getImmediateAliasedSymbol(defaultImportAlias) === exportSymbol); //kill
                    addSearch(name, defaultImportAlias);
                }

                //'default' might be accessed as a property.
                if (!isForRename && exportKind === ExportKind.Default) {
                    Debug.assert(exportName === "default");
                    searchForNamedImport(namedBindings as NamedImports | undefined);
                }
            }
        }

        function searchForNamedImport(namedBindings: NamedImportsOrExports | undefined): void {
            if (!namedBindings) {
                return;
            }

            for (const element of namedBindings.elements) {
                const { name, propertyName } = element;
                if ((propertyName || name).text !== exportName) return;

                if (propertyName) {
                    if (isForRename) { //For a rename, don't continue looking past rename imports. In `import { foo as bar }`, don't touch `bar`, just `foo`.
                        singleReferences.push(propertyName);
                    }
                    else {
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

    //move
    function getSourceFileLikeForImportDeclaration({ parent }: ImportDeclaration | ImportEqualsDeclaration): SourceFileLike {
        if (parent.kind === SyntaxKind.SourceFile) {
            return parent as SourceFile;
        }
        Debug.assert(parent.kind === SyntaxKind.ModuleBlock && parent.parent.kind === SyntaxKind.ModuleDeclaration);
        return parent.parent as ModuleDeclaration;
    }

    //Returns 'true' if the namespace is re-exported from this module.
    function findNamespaceReExports(sourceFileLike: SourceFileLike, name: Identifier, checker: TypeChecker): boolean {
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
    function getAllDirectImports(sourceFiles: SourceFile[], checker: TypeChecker): ImportLike2[][] {
        const map: ImportLike2[][] = [];

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


    function forEachPossibleImportOrExportStatement<T>(sourceFile: SourceFileLike, action: (statement: Statement) => T | undefined): T | undefined { //TODO: no T, just use void
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
    function forEachImport(sourceFile: SourceFile, action: (importStatement: ImportLike2, imported: StringLiteral) => void): void {
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

    //given a match, look for another symbol to search.
    //If we're at an import, look for what it imports.
    //If we're at an export, look for imports of it.
    //exported alrways returns the input symbol, so just return the exportinfo
    export interface ExportInfo {
        exportingModuleSymbol: Symbol;
        exportKind: ExportKind;
    }

    export const enum ImportExport { Import, Export };

    export interface ImportedSymbol {
        kind: ImportExport.Import;
        symbol: Symbol;
        isEqualsOrDefault: boolean;
    }
    export interface ExportedSymbol {
        kind: ImportExport.Export;
        symbol: Symbol;
        info: ExportInfo;
    }
    export function getImportExportSymbols(node: Node, symbol: Symbol, checker: TypeChecker, comingFromExport: boolean): ImportedSymbol | ExportedSymbol | undefined {
        const { parent } = node;

        function exportInfo(symbol: Symbol, kind: ExportKind): ExportedSymbol {
            const info = getExportInfo(symbol, kind, checker);
            return info && { kind: ImportExport.Export, symbol, info }
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
                return exportInfo(symbol, getExportKindForNode(x));
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
        const exportingModuleSymbol = checker.getMergedSymbol(exportSymbol.parent); //need to get merged symbol in case there's an augmentation
        //const moduleDecl = getContainingModule(referenceLocation); //Or just symbol.parent....
        //For export in a namespace, just rely on global search
        return isExternalModuleSymbol(exportingModuleSymbol) ? { exportingModuleSymbol, exportKind } : undefined;
    }

    //This handles default exports. TODO: assert that it returns a result
    function symbolName(symbol: Symbol): string {
        const { name } = symbol;
        if (name !== "default") {
            return name;
        }
        return forEach(symbol.declarations, d => {
            if (d.name && d.name.kind === SyntaxKind.Identifier) {
                return d.name.text;
            }
        });
    }

    export const enum ExportKind { Named, Default, ExportEquals }
    //Not meant for use with export specifiers or export assignment.
    function getExportKindForNode(node: Node): ExportKind | undefined {
        if (hasModifier(node, ModifierFlags.Default)) {
             return ExportKind.Default;
        }
        return ExportKind.Named;
    }
    function skipExportSpecifierSymbol(symbol: Symbol, checker: TypeChecker): Symbol {
        if (symbol.declarations.some(node => node.kind === SyntaxKind.ExportSpecifier && !(node as ExportSpecifier).propertyName)) {
            return checker.getShallowTargetOfExportSpecifier(symbol); //move these calls to one place
        }
        return symbol;
    }
}
