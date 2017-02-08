//TODO: rename this file

/* @internal */
namespace ts.FindAllReferences { //TODO: a different module

    type ImportLike = AnyImportSyntax | ExportDeclaration;


    export interface ImportTracker {
        getImporters(exportingSourceFile: SourceFile, exportKind: ExportKind): Result;
    }
    interface Result {
        // Files that directly access the export.
        directImports: ImportLike[];
        // Files that may (or may not) access the export through a namespace.
        indirectUsers: SourceFile[];
    }

    export function createImportTracker(sourceFiles: SourceFile[], checker: TypeChecker): ImportTracker {
        const allDirectImports = getAllDirectImports(sourceFiles, checker);

        return { getImporters };

        function getImporters(exportingSourceFile: SourceFile, exportKind: ExportKind): Result {
            const directImports = getDirectImports(exportingSourceFile);
            const seenIndirectUsers: Array<true> = [];
            const indirectUsers: SourceFile[] = [];

            // Look for namespace re-exports.
            if (directImports) for (const direct of directImports) {
                switch (direct.kind) {
                    case SyntaxKind.ImportEqualsDeclaration:
                        if (exportKind !== ExportKind.ExportEquals) {
                            handleNamespaceImport(direct.name);
                        }
                        break;

                    case SyntaxKind.ImportDeclaration:
                        const { namedBindings } = direct.importClause;
                        if (namedBindings.kind === SyntaxKind.NamespaceImport) {
                            handleNamespaceImport(namedBindings.name)
                        }
                        break;

                    case SyntaxKind.ExportDeclaration:
                        // Can't access the namespace ere
                        break;
                }
            }

            return { directImports, indirectUsers };

            function handleNamespaceImport(name: Identifier) {
                const source = name.getSourceFile();
                if (findNamespaceReExports(source, name, checker)) {
                    addIndirectUsers(source);
                }
            }

            function addIndirectUsers(sourceFile: SourceFile) {
                const id = getNodeId(sourceFile);
                if (seenIndirectUsers[id]) return;
                seenIndirectUsers[id] = true;
                indirectUsers.push(sourceFile);
                const direct = allDirectImports[id];
                if (direct) for (const d of direct) {
                    addIndirectUsers(d.getSourceFile());
                }
            }
        }

        function getDirectImports(sourceFile: SourceFile): ImportLike[] | undefined {
            return allDirectImports[getNodeId(sourceFile)];
        }
    }

    //Returns 'true' if the namespace is re-exported from this module.
    function findNamespaceReExports(sourceFile: SourceFile, name: Identifier, checker: TypeChecker): boolean {
        const namespaceImportSymbol = checker.getSymbolAtLocation(name);

        return forEachPossibleImportOrExportStatement(sourceFile, statement => {
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

    //Returns a map from source file Ids to import statements.
    function getAllDirectImports(sourceFiles: SourceFile[], checker: TypeChecker): ImportLike[][] {
        const map: ImportLike[][] = [];

        for (const sourceFile of sourceFiles) {
            forEachImport2(sourceFile, (importDecl, moduleSpecifier) => {
                const moduleSymbol = checker.getSymbolAtLocation(moduleSpecifier);
                if (moduleSymbol) {
                    for (const decl of moduleSymbol.declarations) {
                        const source = decl.getSourceFile();

                        const sourceId = getNodeId(source);
                        let imports = map[sourceId];
                        if (!imports) {
                            imports = map[sourceId] = [];
                        }
                        imports.push(importDecl);
                    }
                }
            });
        }

        return map;
    }

    //mostly copied from getImportSearches. Doesn't have to search every import statement, yay.
    export function getImportSearches2(exportSymbol: Symbol, { exportingModuleSymbol, kind: exportKind }: ExportInfo, { createSearch, checker, isForRename, importTracker }: State
        ): { importSearches: Search[], singleReferences: Node[], indirectUsers: SourceFile[] } {
        const exportName = exportSymbol.name;
        const exportingSourceFile = exportingModuleSymbol.declarations[0].getSourceFile(); //neater
        const { directImports, indirectUsers } = importTracker.getImporters(exportingSourceFile, exportKind);

        const importSearches: Search[] = [];
        let singleReferences: Node[] = [];
        function addSearch(location: Node, symbol: Symbol) { importSearches.push(createSearch(location, symbol)); }

        if (directImports) for (const decl of directImports) {
            handleImportLike(decl);
        }

        return { importSearches, singleReferences, indirectUsers };

        function handleImportLike(decl: ImportLike): void {
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
                //An `export =` may be imported by a namespace import. (TODO: TEST!)
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

                if (!isForRename) {
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
                if (propertyName) {
                    if (propertyName.text === exportName) {
                        if (isForRename) { //For a rename, don't continue looking past rename imports. In `import { foo as bar }`, don't touch `bar`, just `foo`.
                            singleReferences.push(propertyName);
                        }
                        else {
                            addSearch(name, checker.getSymbolAtLocation(name));
                        }
                    }
                }
                else if (name.text === exportName) {
                    const localSymbol = element.kind === SyntaxKind.ExportSpecifier ? checker.getExportSpecifierLocalTargetSymbol(element) : checker.getSymbolAtLocation(name);
                    addSearch(name, localSymbol);
                }
            }
        }
    }




    //delete
    export function getImportSearches(importingSourceFile: SourceFile, exportSymbol: Symbol, { exportingModuleSymbol, kind: exportKind }: ExportInfo, { createSearch, checker, isForRename }: State): { importSearches: Search[], singleReferences: Node[] } {
        const exportName = exportSymbol.name;

        const searches: Search[] = [];
        let singleReferences: Node[] = [];
        function addSearch(location: Node, symbol: Symbol) { searches.push(createSearch(location, symbol)); }

        function importsCorrectModule(node: ts.StringLiteral) {
            return checker.getSymbolAtLocation(node) === exportingModuleSymbol;
        }

        forEachImport(importingSourceFile, decl => {
            if (decl.kind === SyntaxKind.ImportEqualsDeclaration) {
                const { name, moduleReference } = decl;
                if (exportKind === ExportKind.ExportEquals &&
                    moduleReference.kind === SyntaxKind.ExternalModuleReference &&
                    moduleReference.expression.kind === SyntaxKind.StringLiteral &&
                    importsCorrectModule(moduleReference.expression as StringLiteral)) {

                    if (!isForRename || decl.name.text === exportSymbol.name) {
                        addSearch(name, checker.getSymbolAtLocation(name));
                    }
                }
                return;
            }

            const { moduleSpecifier } = decl;
            Debug.assert(moduleSpecifier.kind === SyntaxKind.StringLiteral);
            if (!importsCorrectModule(moduleSpecifier as StringLiteral)) {
                return;
            }

            if (decl.kind === SyntaxKind.ExportDeclaration) {
                searchForNamedImport(decl.exportClause);
                return;
            }

            const { importClause } = decl;

            const { namedBindings } = importClause;
            if (namedBindings && namedBindings.kind === ts.SyntaxKind.NamespaceImport) {
                //`import * as x from "./x"
                //Text search will catch this (must use the name)
                //An `export =` may be imported by a namespace import. (TODO: TEST!)
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

                if (!isForRename) {
                    Debug.assert(exportName === "default");
                    searchForNamedImport(namedBindings as NamedImports | undefined);
                }
            }
        });

        return { importSearches: searches, singleReferences };

        function searchForNamedImport(namedBindings: NamedImportsOrExports | undefined): void {
            if (!namedBindings) {
                return;
            }

            for (const { name, propertyName } of namedBindings.elements) {
                if (propertyName) {
                    if (propertyName.text === exportName) {
                        if (isForRename) { //For a rename, don't continue looking past rename imports. In `import { foo as bar }`, don't touch `bar`, just `foo`.
                            singleReferences.push(propertyName);
                        }
                        else {
                            addSearch(name, checker.getSymbolAtLocation(name));
                        }
                    }
                }
                else if (name.text === exportName) {
                    //Already checked that module symbol matches.
                    addSearch(name, checker.getSymbolAtLocation(name));
                }
            }
        }
    }

    function forEachPossibleImportOrExportStatement<T>(sourceFile: SourceFile, action: (statement: Statement) => T | undefined): T | undefined { //TODO: no T, just use void
        for (const statement of sourceFile.statements) {
            const x = action(statement);
            if (x !== undefined) return x;

            if (statement.kind === SyntaxKind.ModuleDeclaration) {
                const decl = statement as ModuleDeclaration;
                if (decl.name.kind === SyntaxKind.StringLiteral) {
                    for (const statement of (decl.body as ModuleBlock).statements) {
                        const x = action(statement);
                        if (x !== undefined) return x;
                    }
                }
            }
        }
    }

    function forEachImport2(sourceFile: SourceFile, action: (importStatement: ImportLike, imported: StringLiteral) => void): void {
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
                        Debug.assert(decl.moduleSpecifier.kind === SyntaxKind.StringLiteral);
                        action(decl, decl.moduleSpecifier as StringLiteral);
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
        }
    }

    function forEachImport(sourceFile: SourceFile, action: (importDeclaration: AnyImportSyntax | ExportDeclaration) => void): void {
        if (sourceFile.externalModuleIndicator) {
            for (const moduleSpecifier of sourceFile.imports) {
                let importDecl = moduleSpecifier.parent;
                if (importDecl.kind !== SyntaxKind.ImportDeclaration && importDecl.kind !== SyntaxKind.ExportDeclaration) {
                    Debug.assert(importDecl.kind === SyntaxKind.ExternalModuleReference);
                    importDecl = importDecl.parent;
                    Debug.assert(importDecl.kind === SyntaxKind.ImportEqualsDeclaration);
                }
                action(importDecl as AnyImportSyntax | ExportDeclaration);
            }
        }
        else {
            // Declaration file or global script may have module declarations with imports inside of them, so must recurse.
            forEachImportInNode(sourceFile, action);
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
        kind: ExportKind;
    }

    //name
    interface IImported {
        symbol: Symbol;
        isEqualsOrDefault: boolean;
    }
    interface IExported {
        symbol: Symbol;
        info: ExportInfo;
    }
    //todo: need export-import tests
    export function getImportExportSymbols(node: Node, symbol: Symbol, checker: TypeChecker): { imported?: IImported, exported?: IExported } {
        let imported: IImported | undefined;
        let exported: IExported | undefined;
        const { parent } = node;

        function exportInfo(symbol: Symbol, kind: ExportKind): IExported {
            const info = getExportInfo(symbol, kind);
            return info && { symbol, info }
        }

        if (symbol.flags & SymbolFlags.Export) {
            if (symbol.declarations.some(d => d === parent)) {
                switch (getSpecialPropertyAssignmentKind(parent.parent)) {
                    case SpecialPropertyAssignmentKind.ExportsProperty:
                        return { exported: exportInfo(symbol, ExportKind.Named) };
                    case SpecialPropertyAssignmentKind.ModuleExports:
                        //TODO:TEST
                        return { exported: exportInfo(symbol, ExportKind.ExportEquals) };
                    case SpecialPropertyAssignmentKind.PrototypeProperty:
                    case SpecialPropertyAssignmentKind.ThisProperty:
                        return {};
                    case SpecialPropertyAssignmentKind.None:
                        //test
                        const { exportSymbol } = symbol;
                        Debug.assert(!!exportSymbol);
                        exported = exportInfo(exportSymbol, getExportKindForNode(parent));
                        break;
                }
            }
        } else {
            const x = getExportNodeFromNodeNodeNode(parent);
            if (hasModifier(x, ModifierFlags.Export)) {
                exported = exportInfo(symbol, getExportKindForNode(x));
            }
            else if (parent.kind === SyntaxKind.ExportAssignment) {
                // Get the symbol for the `export =` node; its parent is the module it's the export of.
                const exportingModuleSymbol = parent.symbol.parent;
                Debug.assert(!!exportingModuleSymbol);
                return { exported: { symbol, info: { exportingModuleSymbol, kind: ExportKind.ExportEquals } } }
            }
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
                    imported = { symbol: importedSymbol, ...isImport };
                }
            }
        }

        return { imported, exported };
    }

    //Looks like we don't need isEqualsOrDefault!
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

    //move
    //undefined means: ignore this export
    export function getExportInfo(exportSymbol: Symbol, exportKind: ExportKind): ExportInfo | undefined {
        const exportingModuleSymbol = exportSymbol.parent;
        //const moduleDecl = getContainingModule(referenceLocation); //Or just symbol.parent....
        //For export in a namespace, just rely on global search
        return isExternalModuleSymbol(exportingModuleSymbol) ? { exportingModuleSymbol, kind: exportKind } : undefined;
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
        if (symbol.declarations.some(isShallowExportSpecifier)) {
            return checker.getShallowTargetOfExportSpecifier(symbol); //move these calls to one place
        }
        return symbol;
    }
    function isShallowExportSpecifier(node: Node) {
        return node.kind === SyntaxKind.ExportSpecifier && !(node as ExportSpecifier).propertyName;
    }
}
