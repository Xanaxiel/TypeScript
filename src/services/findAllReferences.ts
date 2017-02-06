/* @internal */
namespace ts.FindAllReferences {
    export function findReferencedSymbols(checker: TypeChecker, cancellationToken: CancellationToken, sourceFiles: SourceFile[], sourceFile: SourceFile, position: number): ReferencedSymbol[] | undefined {
        const referencedSymbols = findAllReferencedSymbols(checker, cancellationToken, sourceFiles, sourceFile, position);
        // Only include referenced symbols that have a valid definition.
        return filter(referencedSymbols, rs => !!rs.definition);
    }

    export function getImplementationsAtPosition(typeChecker: TypeChecker, cancellationToken: CancellationToken, sourceFiles: SourceFile[], sourceFile: SourceFile, position: number): ImplementationLocation[] {
        const node = getTouchingPropertyName(sourceFile, position);
        const referenceEntries = getImplementationReferenceEntries(typeChecker, cancellationToken, sourceFiles, node);
        return map(referenceEntries, ({ textSpan, fileName }) => ({ textSpan, fileName }));
    }

    function getImplementationReferenceEntries(typeChecker: TypeChecker, cancellationToken: CancellationToken, sourceFiles: SourceFile[], node: Node): ReferenceEntry[] {
        // If invoked directly on a shorthand property assignment, then return
        // the declaration of the symbol being assigned (not the symbol being assigned to).
        if (node.parent.kind === SyntaxKind.ShorthandPropertyAssignment) {
            const result: ReferenceEntry[] = [];
            getReferenceEntriesForShorthandPropertyAssignment(node, typeChecker, node =>
                result.push(getReferenceEntryFromNode(node)));
            return result;
        }
        else if (node.kind === SyntaxKind.SuperKeyword || isSuperProperty(node.parent)) {
            // References to and accesses on the super keyword only have one possible implementation, so no
            // need to "Find all References"
            const symbol = typeChecker.getSymbolAtLocation(node);
            return symbol.valueDeclaration && [getReferenceEntryFromNode(symbol.valueDeclaration)];
        }
        else {
            // Perform "Find all References" and retrieve only those that are implementations
            return getReferenceEntriesForNode(node, sourceFiles, typeChecker, cancellationToken, { implementations: true });
        }
    }

    //function undefinedIfEmpty<T>(arr: T[] | undefined): T[] | undefined {
    //    return arr && arr.length ? arr : undefined;
    //}
    export function findReferencedEntries(checker: TypeChecker, cancellationToken: CancellationToken, sourceFiles: SourceFile[], sourceFile: SourceFile, position: number, options?: Options): ReferenceEntry[] | undefined {
        return convertReferences(findAllReferencedSymbols(checker, cancellationToken, sourceFiles, sourceFile, position, options));
    }

    function findAllReferencedSymbols(checker: TypeChecker, cancellationToken: CancellationToken, sourceFiles: SourceFile[], sourceFile: SourceFile, position: number, options?: Options): ReferencedSymbol[] | undefined {
        const node = getTouchingPropertyName(sourceFile, position, /*includeJsDocComment*/ true);
        return getReferencedSymbolsForNode(node, sourceFiles, checker, cancellationToken,options);
    }

    export function getReferenceEntriesForNode(node: Node, sourceFiles: SourceFile[], checker: TypeChecker, cancellationToken: CancellationToken, options: Options = {}): ReferenceEntry[] | undefined {
        return convertReferences(getReferencedSymbolsForNode(node, sourceFiles, checker, cancellationToken, options));
    }

    function convertReferences(referenceSymbols: ReferencedSymbol[]): ReferenceEntry[] {
        return referenceSymbols && flatMap(referenceSymbols, r => r.references);
    }

    function getReferencedSymbolsForNode(node: Node, sourceFiles: SourceFile[], checker: TypeChecker, cancellationToken: CancellationToken, options: Options = {}): ReferencedSymbol[] | undefined {
        if (node.kind === ts.SyntaxKind.SourceFile) {
            return undefined;
        }

        if (!options.implementations) {
            const special = getReferencedSymbolsSpecial(node, sourceFiles, checker, cancellationToken);
            if (special) {
                return special;
            }
        }

        // `getSymbolAtLocation` normally returns the symbol of the class when given the constructor keyword,
        // so we have to specify that we want the constructor symbol.
        const symbol = checker.getSymbolAtLocation(node);

        // Could not find a symbol e.g. unknown identifier
        if (!symbol) {
            // String literal might be a property, so do this here rather than in getReferencedSymbolsSpecial.
            if (!options.implementations && node.kind === SyntaxKind.StringLiteral) {
                return getReferencesForStringLiteral(<StringLiteral>node, sourceFiles, checker, cancellationToken);
            }
            // Can't have references to something that we have no symbol for.
            return undefined;
        }

        // The symbol was an internal symbol and does not have a declaration e.g. undefined symbol
        if (!symbol.declarations || !symbol.declarations.length) {
            return undefined;
        }

        return findYouSomeReferencesForGreatGood(symbol, node, sourceFiles, checker, cancellationToken, options);
    }

    export interface Options {
        readonly findInStrings?: boolean;
        readonly findInComments?: boolean;
        readonly isForRename?: boolean;
        readonly implementations?: boolean;
    }

    //this changes every time
    interface Search {
        readonly location: Node; //This is used to generate display parts for the definition. Use State.originalLocation for other things.
        readonly symbol: Symbol;
        readonly text: string;
        readonly escapedText: string;

        includes(symbol: Symbol): boolean;
    }

    interface State extends Options {
        readonly originalLocation: Node;

        readonly sourceFiles: SourceFile[];
        readonly checker: TypeChecker;
        readonly cancellationToken: CancellationToken;
        readonly inheritsFromCache: Map<boolean>;
        readonly searchMeaning: SemanticMeaning;

        //Returns 'true' if we've already visited.
        markSearched(sourceFile: SourceFile, symbol: Symbol): boolean;

        // Type nodes can contain multiple references to the same type. For example:
        //      let x: Foo & (Foo & Bar) = ...
        // Because we are returning the implementation locations and not the identifier locations,
        // duplicate entries would be returned here as each of the type references is part of
        // the same implementation. For that reason, check before we add a new entry
        markSeenContainingTypeReference(containingTypeReference: Node): void;

        addStringOrCommentReference(fileName: string, textSpan: TextSpan): void;
        getReferencePusher(referenceSymbol: Symbol, referenceLocation: Node): (node: Node) => void; //Make this private!
        addReferences(search: Search, references: Node[]): void;
        createSearch(location: Node, symbol: Symbol, symbols?: Symbol[]): Search;
    }

    function createState(sourceFiles: SourceFile[], originalLocation: Node, checker: TypeChecker, cancellationToken: CancellationToken, searchMeaning: SemanticMeaning, options: Options, result: ReferencedSymbol[]): State {
        const symbolToIndex: number[] = [];
        const inheritsFromCache = createMap<boolean>();
        //Maps source file -> symbol Id -> true
        const sourceFileToSeenSymbols: Array<Array<true>> = [];
        const seenContainingTypeReferences: Array<true> = [];

        return { sourceFiles, originalLocation, checker, cancellationToken, searchMeaning, inheritsFromCache, ...options,
            markSearched, markSeenContainingTypeReference, addStringOrCommentReference, getReferencePusher, addReferences, createSearch };

        function markSearched(sourceFile: SourceFile, symbol: Symbol): boolean {
            const sourceId = getNodeId(sourceFile);
            const symbolId = getSymbolId(symbol);
            let seenSymbols = sourceFileToSeenSymbols[sourceId] || (sourceFileToSeenSymbols[sourceId] = []);
            if (seenSymbols[symbolId]) {
                return true;
            }
            seenSymbols[symbolId] = true;
            return false;
        }

        function markSeenContainingTypeReference(containingTypeReference: Node): boolean {
            const id = getNodeId(containingTypeReference);
            if (seenContainingTypeReferences[id]) {
                return true;
            }
            seenContainingTypeReferences[id] = true;
            return false;
        }

        function addStringOrCommentReference(fileName: string, textSpan: TextSpan) {
            result.push({
                definition: undefined,
                references: [{ fileName, textSpan, isWriteAccess: false, isDefinition: false }]
            });
        }

        function getReferencePusher(referenceSymbol: Symbol, referenceLocation: Node): (node: Node) => void {
            return node => getReferencedSymbol(referenceSymbol, referenceLocation).references.push(getReferenceEntryFromNode(node));
        }

        //private?
        function getReferencedSymbol(symbol: Symbol, searchLocation: Node): ReferencedSymbol {
            const symbolId = getSymbolId(symbol);
            let index = symbolToIndex[symbolId];
            if (index === undefined) {
                index = result.length;
                symbolToIndex[symbolId] = index;

                result.push({
                    definition: getDefinition(symbol, searchLocation, checker),
                    references: []
                });
            }

            return result[index];
        }

        //Just take the Search as a parameter.
        function addReferences(search: Search, references: Node[]): void {
            if (references.length) {
                const addReference = getReferencePusher(search.symbol, search.location);
                for (const ref of references) {
                    addReference(ref);
                }
            }
        }

        function createSearch(location: Node, symbol: Symbol, symbols: Symbol[] = [symbol]): Search {
            // Get the text to search for.
            // Note: if this is an external module symbol, the name doesn't include quotes.
            //TODO: probably don't need to strip quotes?
            const text = stripQuotes(getDeclaredName(checker, symbol, location));
            const escapedText = escapeIdentifier(text);
            return { location, symbol, text, escapedText, includes };

            function includes(symbol: Symbol): boolean {
                return contains(symbols, symbol);
            }
        }
    }

    //name: this does the whole thing
    //symbol = symbol at location of 'node'
    function findYouSomeReferencesForGreatGood(symbol: Symbol, node: Node, sourceFiles: SourceFile[], checker: TypeChecker, cancellationToken: CancellationToken, options: Options): ReferencedSymbol[] {
        //special-case: In `var foo; export { foo }` there are 2 different symbols, but only test for 'var foo'.
        let exportInfo: ExportInfo | undefined;

        // If we're at an `export { foo }` symbol, don't start the search with the exported symbol, search with the local symbol.
        if (parentIsShorthandExportSpecifier(node)) {
            symbol = checker.getShallowTargetOfExportSpecifier(symbol);
        }

        // Try to get the smallest valid scope that we can limit our search to;
        // otherwise we'll need to search globally (i.e. include each file).
        const scope = getSymbolScope(symbol, node);

        //Build the set of symbols to search for, initially it has only the current symbol
        const searchSymbols = populateSearchSymbolSet(symbol, node, checker, options.implementations);
        //if (shorthandModuleSymbol) {
        //    searchSymbols.push(shorthandModuleSymbol);
        //}

        // Compute the meaning from the location and the symbol it references
        const searchMeaning = getIntersectingMeaningFromDeclarations(getMeaningFromLocation(node), symbol.declarations);

        const result: ReferencedSymbol[] = [];
        const state = createState(sourceFiles, node, checker, cancellationToken, searchMeaning, options, result);
        const search = state.createSearch(node, symbol, searchSymbols);

        if (scope) {
            getReferencesInContainer(scope, search, state);
        }
        else {
            findAllRefsGloballyOrExported(search, state, exportInfo);
        }

        return result;
    }

    //move
    function getExportKindForNode(node: Node): ExportKind | undefined {
        if (hasModifier(node, ModifierFlags.Default)) {
             return ExportKind.Default;
        }
        if (node.kind === SyntaxKind.ExportAssignment) {
            return ExportKind.ExportEquals;
        }
        return ExportKind.Named;
    }

    interface ExportInfo { moduleSymbol: Symbol, kind: ExportKind }
    function findAllRefsGloballyOrExported(search: Search, state: State, exportInfo?: ExportInfo) {
        for (const sourceFile of state.sourceFiles) {
            //Previously didn't want to add a reference to the export itself.
            //if (sourceFile === moduleFile) {
            //    continue;
            //}
            state.cancellationToken.throwIfCancellationRequested();

            if (exportInfo !== undefined) {
                const { localSearches, singles } = getImportSearches(sourceFile, search.symbol, exportInfo, state); //TODO: get a combined search?
                if (singles) { state.addReferences(search, singles); }
                for (const search of localSearches) {
                    getReferencesInContainer(sourceFile, search, state);
                } //TODO: only do this for wierdly-named imports, so we don't duplicate work... although getReferencesInContainer checks for that too....
            }

            //Also, it might be available as a named property.
            //TODO: if a default export, look for '.default'!
            //NOTE: we might be able to detect this based on whether any 'import *' exists anywhere...
            getInternedName; //Pretty sure don't need this any more!
            if (sourceFileHasName(sourceFile, search.escapedText)) {
                getReferencesInContainer(sourceFile, search, state);
            }
        }
    }

    /** getReferencedSymbols for special node kinds. */
    function getReferencedSymbolsSpecial(node: Node, sourceFiles: SourceFile[], checker: TypeChecker, cancellationToken: CancellationToken): ReferencedSymbol[] | undefined {
        if (isTypeKeyword(node.kind)) {
            return getAllReferencesForKeyword(sourceFiles, node.kind, cancellationToken);
        }

        // Labels
        if (isLabelName(node)) {
            if (isJumpStatementTarget(node)) {
                const labelDefinition = getTargetLabel((<BreakOrContinueStatement>node.parent), (<Identifier>node).text);
                // if we have a label definition, look within its statement for references, if not, then
                // the label is undefined and we have no results..
                return labelDefinition && getLabelReferencesInNode(labelDefinition.parent, labelDefinition, cancellationToken);
            }
            else {
                // it is a label definition and not a target, search within the parent labeledStatement
                return getLabelReferencesInNode(node.parent, <Identifier>node, cancellationToken);
            }
        }

        if (isThis(node)) {
            return getReferencesForThisKeyword(node, sourceFiles, checker, cancellationToken);
        }

        if (node.kind === SyntaxKind.SuperKeyword) {
            return getReferencesForSuperKeyword(node, checker, cancellationToken);
        }

        return undefined;
    }

    function sourceFileHasName(sourceFile: SourceFile, escapedName: string): boolean {
        return getNameTable(sourceFile).get(escapedName) !== undefined;
    }

    const enum ExportKind { Named, Default, ExportEquals }

    //TODO: may be imported more than once, so return multiple results!!! TEST
    //TODO: decided I don't need moduleReExports...
    //REMEMBER: 'export =' might be imported by a default or namespace import!!!!!!!!!!!!!!!! TEST
    //TODO: just take the exportinfo as a parameter
    function getImportSearches(importingSourceFile: SourceFile, exportSymbol: Symbol, { moduleSymbol: exportingModuleSymbol, kind: exportKind }: ExportInfo, { createSearch, checker, isForRename }: State): { localSearches: Search[], moduleReExports: Symbol[], singles: Node[] } {
        const exportName = exportSymbol.name;

        const searches: Search[] = [];
        let singles: Node[] = [];
        function addSearch(location: Node, symbol: Symbol) { searches.push(createSearch(location, symbol)); }

        const reExports: Symbol[] = []; //!!!

        function importsCorrectModule(node: ts.StringLiteral) {
            return checker.getSymbolAtLocation(node) === exportingModuleSymbol;
        }

        forEachImport(importingSourceFile, decl => {
            //Note:
            if (decl.kind === SyntaxKind.ImportEqualsDeclaration) { //TEST
                if (exportKind !== ExportKind.ExportEquals) {
                    return;
                }

                const { moduleReference } = decl;
                if (moduleReference.kind === SyntaxKind.ExternalModuleReference &&
                    moduleReference.expression.kind === SyntaxKind.StringLiteral &&
                    importsCorrectModule(moduleReference.expression as StringLiteral)) {
                    //`import x = require("./x")`.
                    //Search for the local `x`.
                    const location = decl.name;
                    addSearch(location, checker.getSymbolAtLocation(location));
                }
                return;
            }

            const { importClause, moduleSpecifier } = decl;

            /*if (moduleSpecifier.kind === SyntaxKind.ExternalModuleReference) {
                const { expression } = moduleSpecifier as ExternalModuleReference;
                if (expression.kind === SyntaxKind.StringLiteral && checker.getSymbolAtLocation(expression) === exportingModuleSymbol) {
                    // `import x = require("./x")`, so use the original symbol and search for its appearance as a property.
                    //TODO: but if `export =` a function, this is an alias instead!!!
                    //So don't have `isDefault`, have `enum ExportKind { Default, ExportEquals, Named }`
                    addSearch(exportLocation, exportSymbol);
                    return;
                }
            }*/

            Debug.assert(moduleSpecifier.kind === SyntaxKind.StringLiteral);

            if (!importsCorrectModule(moduleSpecifier as StringLiteral)) {
                return;
            }

            const { namedBindings } = importClause;
            if (namedBindings && namedBindings.kind === ts.SyntaxKind.NamespaceImport) {
                //`import * as x from "./x"
                //Text search will catch this (must use the name)
                //TODO: remember to test for '.default'...

                //An `export =` may be imported by a namespace import. (TODO: TEST!)
                if (exportKind === ExportKind.ExportEquals) {
                    const location = namedBindings.name;
                    addSearch(location, checker.getSymbolAtLocation(location)); //duplicate code
                }

                return;
            }

            if (exportKind === ExportKind.Named) {
                if (namedBindings) {
                    for (const { name, propertyName } of (namedBindings as NamedImports).elements) {
                        if (propertyName && propertyName.text === exportName) {
                            //Want to just include the propertyName node as a reference, *not* the renamed element. Important so we don't rename too much.
                            //Change `import { foo as bar }` to `import { oof as bar }` and leave the rest alone!
                            if (isForRename) {
                                singles.push(propertyName); //test that renaming works!!!
                            }
                            else {
                                addSearch(name, checker.getSymbolAtLocation(name)); //dup of below
                            }

                        }
                        if (name.text === exportName) {
                            addSearch(name, checker.getSymbolAtLocation(name));
                        }
                    }
                }
            }
            else {
                //`export =` might be imported by a default import if `--allowSyntheticDefaultExports` is on, so this handles both ExportKind.Default and ExportKind.ExportEquals
                const { name } = importClause;
                if (!name) {
                    return;
                }
                const defaultImportAlias = checker.getSymbolAtLocation(name);
                if (checker.getImmediateAliasedSymbol(defaultImportAlias) === exportSymbol) {
                    addSearch(name, defaultImportAlias);
                }
            }
        });

        return { localSearches: searches, moduleReExports: reExports, singles };
    }

    //TODO: handle export-import
    function forEachImport(sourceFile: SourceFile | ModuleBlock, action: (importDeclaration: ImportDeclaration | ImportEqualsDeclaration) => void) {
        //for (const importSpecifier of sourceFile.imports) {
        //    action(importSpecifier);
        //}

        //Look for nested imports
        for (const statement of sourceFile.statements) {
            switch (statement.kind) {
                case SyntaxKind.ImportDeclaration:
                case SyntaxKind.ImportEqualsDeclaration:
                    action(statement as ImportDeclaration | ImportEqualsDeclaration);
                    break;

                case SyntaxKind.ModuleDeclaration: {
                    const decl = statement as ModuleDeclaration;
                    //No imports in namespaces, right???
                    if (decl.name.kind === SyntaxKind.StringLiteral) {
                        forEachImport(decl.body as ModuleBlock, action);
                    }
                }
            }
        }
    }

    function getDefinition(symbol: Symbol, node: Node, checker: TypeChecker): ReferencedSymbolDefinitionInfo {
        const declarations = symbol.declarations;
        if (!declarations || declarations.length === 0) {
            return undefined;
        }

        //const decl = symbol.declarations[0];
        const { displayParts, symbolKind } =
            SymbolDisplay.getSymbolDisplayPartsDocumentationAndSymbolKind(checker, symbol, node.getSourceFile(), getContainerNode(node), node);
            //SymbolDisplay.getSymbolDisplayPartsDocumentationAndSymbolKind(typeChecker, symbol, decl.getSourceFile(), getContainerNode(decl), decl);
        const name = displayParts.map(p => p.text).join("");
        return {
            containerKind: "",
            containerName: "",
            name,
            kind: symbolKind,
            fileName: declarations[0].getSourceFile().fileName,
            textSpan: createTextSpan(declarations[0].getStart(), 0),
            displayParts
        };
    }

    function getPropertySymbolOfDestructuringAssignment(location: Node, checker: TypeChecker) {
        return isArrayLiteralOrObjectLiteralDestructuringPattern(location.parent.parent) &&
            checker.getPropertySymbolOfDestructuringAssignment(<Identifier>location);
    }

    function isObjectBindingPatternElementWithoutPropertyName(symbol: Symbol) {
        const bindingElement = <BindingElement>getDeclarationOfKind(symbol, SyntaxKind.BindingElement);
        return bindingElement &&
            bindingElement.parent.kind === SyntaxKind.ObjectBindingPattern &&
            !bindingElement.propertyName;
    }

    function getPropertySymbolOfObjectBindingPatternWithoutPropertyName(symbol: Symbol, checker: TypeChecker) {
        if (isObjectBindingPatternElementWithoutPropertyName(symbol)) {
            const bindingElement = <BindingElement>getDeclarationOfKind(symbol, SyntaxKind.BindingElement);
            const typeOfPattern = checker.getTypeAtLocation(bindingElement.parent);
            return typeOfPattern && checker.getPropertyOfType(typeOfPattern, (<Identifier>bindingElement.name).text);
        }
        return undefined;
    }

    function getInternedName(symbol: Symbol, location: Node): string { //rename this fn
        //If this is an export or import specifier it could have been renamed using the 'as' syntax.
        //If so we want to search for whatever under the cursor.
        if (isImportOrExportSpecifierName(location)) {
            return location.text;
        }

        return stripQuotes(symbol.name);
    }

    /**
     * Determines the smallest scope in which a symbol may have named references.
     * Note that not every construct has been accounted for. This function can
     * probably be improved.
     *
     * @returns undefined if the scope cannot be determined, implying that
     * a reference to a symbol can occur anywhere.
     */
    function getSymbolScope(symbol: Symbol, _node: Node): Node | undefined { //TODO: kill _node
        // If this is the symbol of a named function expression or named class expression,
        // then named references are limited to its own scope.
        const valueDeclaration = symbol.valueDeclaration;
        if (valueDeclaration && (valueDeclaration.kind === SyntaxKind.FunctionExpression || valueDeclaration.kind === SyntaxKind.ClassExpression)) {
            return valueDeclaration;
        }

        // If this is private property or method, the scope is the containing class
        if (symbol.flags & (SymbolFlags.Property | SymbolFlags.Method)) {
            const privateDeclaration = find(symbol.getDeclarations(), d => !!(getModifierFlags(d) & ModifierFlags.Private)); //TODO:helper...
            if (privateDeclaration) {
                return getAncestor(privateDeclaration, SyntaxKind.ClassDeclaration);
            }
        }

        //If the symbol is an import we would like to find it if we are looking for what it imports.
        //So consider it visible outside its declaration scope.
        //if (symbol.flags & SymbolFlags.Alias) {
        //    return undefined;
        //}
        //ACTUALLY, we will do this later

        // If symbol is of object binding pattern element without property name we would want to
        // look for property too and that could be anywhere
        if (isObjectBindingPatternElementWithoutPropertyName(symbol)) {
            return undefined;
        }

        // if symbol correspond to the union property - bail out
        if (symbol.flags & SymbolFlags.SyntheticProperty) {
            return undefined;
        }

        //If it's a property of something, need to search globally.
        //If it's a symbol on a module, we will recurse once we see an export.
        //For UMD export, its symbol.parent is the module it aliases, and symbol.parent.globalExports will be set., we search globally of course.
        if (symbol.parent && (!(symbol.parent.flags & SymbolFlags.Module && isExternalModuleSymbol(symbol.parent))) || isUmdSymbol(symbol) ) {
            return undefined;
        }

        const declarations = symbol.getDeclarations();
        if (!declarations) {
            return undefined;
        }

        let scope: Node | undefined;
        for (const declaration of declarations) {
            const container = getContainerNode(declaration);
            if (scope && scope !== container) {
                // Different declarations have different containers, bail out
                return undefined;
            }

            if (!container || container.kind === SyntaxKind.SourceFile && !isExternalModuleLike(<SourceFile>container)) {
                // This is a global variable and not an external module, any declaration defined
                // within this scope is visible outside the file
                return undefined;
            }

            // The search scope is the container node
            scope = container;
        }

        //If symbol.parent it means that we saw symbol.parent.flags & SymbolFlags.Module above (else would immediately return undefined)
        //If symbol.parent, this is an export from a module *or namespace*. For an export from a namespace, check the entire source file.
        return symbol.parent ? scope.getSourceFile() : scope;
    }

    function isUmdSymbol(symbol: Symbol) {
        return symbol.declarations.some(node => node.kind === SyntaxKind.NamespaceExportDeclaration);
    }

    function getPossibleSymbolReferencePositions(sourceFile: SourceFile, symbolName: string, start: number, end: number, cancellationToken: CancellationToken): number[] {
        const positions: number[] = [];

        /// TODO: Cache symbol existence for files to save text search
        // Also, need to make this work for unicode escapes.

        // Be resilient in the face of a symbol with no name or zero length name
        if (!symbolName || !symbolName.length) {
            return positions;
        }

        const text = sourceFile.text;
        const sourceLength = text.length;
        const symbolNameLength = symbolName.length;

        let position = text.indexOf(symbolName, start);
        while (position >= 0) {
            cancellationToken.throwIfCancellationRequested();

            // If we are past the end, stop looking
            if (position > end) break;

            // We found a match.  Make sure it's not part of a larger word (i.e. the char
            // before and after it have to be a non-identifier char).
            const endPosition = position + symbolNameLength;

            if ((position === 0 || !isIdentifierPart(text.charCodeAt(position - 1), ScriptTarget.Latest)) &&
                (endPosition === sourceLength || !isIdentifierPart(text.charCodeAt(endPosition), ScriptTarget.Latest))) {
                // Found a real match.  Keep searching.
                positions.push(position);
            }
            position = text.indexOf(symbolName, position + symbolNameLength + 1);
        }

        return positions;
    }

    function getLabelReferencesInNode(container: Node, targetLabel: Identifier, cancellationToken: CancellationToken): ReferencedSymbol[] {
        const references: ReferenceEntry[] = [];
        const sourceFile = container.getSourceFile();
        const labelName = targetLabel.text;
        const possiblePositions = getPossibleSymbolReferencePositions(sourceFile, labelName, container.getStart(), container.getEnd(), cancellationToken);
        forEach(possiblePositions, position => {
            cancellationToken.throwIfCancellationRequested();

            const node = getTouchingWord(sourceFile, position);
            if (!node || node.getWidth() !== labelName.length) {
                return;
            }

            // Only pick labels that are either the target label, or have a target that is the target label
            if (node === targetLabel ||
                (isJumpStatementTarget(node) && getTargetLabel(node, labelName) === targetLabel)) {
                references.push(getReferenceEntryFromNode(node));
            }
        });

        const definition: ReferencedSymbolDefinitionInfo = {
            containerKind: "",
            containerName: "",
            fileName: targetLabel.getSourceFile().fileName,
            kind: ScriptElementKind.label,
            name: labelName,
            textSpan: createTextSpanFromNode(targetLabel, sourceFile),
            displayParts: [displayPart(labelName, SymbolDisplayPartKind.text)]
        };

        return [{ definition, references }];
    }

    function isValidReferencePosition(node: Node, searchSymbolName: string): boolean {
        // Compare the length so we filter out strict superstrings of the symbol we are looking for
        switch (node && node.kind) {
            case SyntaxKind.Identifier:
                return node.getWidth() === searchSymbolName.length;

            case SyntaxKind.StringLiteral:
                return (isLiteralNameOfPropertyDeclarationOrIndexAccess(node) || isNameOfExternalModuleImportOrDeclaration(node)) &&
                    // For string literals we have two additional chars for the quotes
                    node.getWidth() === searchSymbolName.length + 2;

            case SyntaxKind.NumericLiteral:
                return isLiteralNameOfPropertyDeclarationOrIndexAccess(node) && node.getWidth() === searchSymbolName.length;

            default:
                return false;
        }
    }

    function getAllReferencesForKeyword(sourceFiles: SourceFile[], keywordKind: ts.SyntaxKind, cancellationToken: CancellationToken): ReferencedSymbol[] {
        const name = tokenToString(keywordKind);
        const definition: ReferencedSymbolDefinitionInfo = {
            containerKind: "",
            containerName: "",
            fileName: "",
            kind: ScriptElementKind.keyword,
            name,
            textSpan: createTextSpan(0, 1),
            displayParts: [{ text: name, kind: ScriptElementKind.keyword }]
        }
        const references: ReferenceEntry[] = [];
        for (const sourceFile of sourceFiles) {
            cancellationToken.throwIfCancellationRequested();
            addReferencesForKeywordInFile(sourceFile, keywordKind, name, cancellationToken, references);
        }

        return [{ definition, references }];
    }

    function addReferencesForKeywordInFile(sourceFile: SourceFile, kind: SyntaxKind, searchText: string, cancellationToken: CancellationToken, references: Push<ReferenceEntry>): void {
        const possiblePositions = getPossibleSymbolReferencePositions(sourceFile, searchText, sourceFile.getStart(), sourceFile.getEnd(), cancellationToken);
        for (const position of possiblePositions) {
            cancellationToken.throwIfCancellationRequested();
            const referenceLocation = getTouchingPropertyName(sourceFile, position);
            if (referenceLocation.kind === kind) {
                references.push({
                    textSpan: createTextSpanFromNode(referenceLocation),
                    fileName: sourceFile.fileName,
                    isWriteAccess: false,
                    isDefinition: false,
                });
            }
        }
    }

    /** Search within node "container" for references for a search value, where the search value is defined as a
         * tuple of(searchSymbol, searchText, searchLocation, and searchMeaning).
        * searchLocation: a node where the search value
        */
    //This gets references within a scope.
    //TOOD: 'checkMark' is ugly...
    function getReferencesInContainer(container: Node, search: Search, state: State): void {
        const sourceFile = container.getSourceFile();
        if (state.markSearched(sourceFile, search.symbol)) { //Uh, but we've only searched in the container, not in the whole source file...
            return;
        }

        const start = state.findInComments ? container.getFullStart() : container.getStart();
        const possiblePositions = getPossibleSymbolReferencePositions(sourceFile, search.text, start, container.getEnd(), state.cancellationToken);
        const parentSymbols = state.implementations ? getParentSymbolsOfPropertyAccess() : undefined; //this is just for 'implementations'
        for (const position of possiblePositions) {
            state.cancellationToken.throwIfCancellationRequested();
            getReferencesAtLocation(sourceFile, position, search, parentSymbols, state);
        }

        /* If we are just looking for implementations and this is a property access expression, we need to get the
         * symbol of the local type of the symbol the property is being accessed on. This is because our search
         * symbol may have a different parent symbol if the local type's symbol does not declare the property
         * being accessed (i.e. it is declared in some parent class or interface)
         */
        function getParentSymbolsOfPropertyAccess(): Symbol[] | undefined {
            const propertyAccessExpression = getPropertyAccessExpressionFromRightHandSide(search.location);
            if (!propertyAccessExpression) {
                return undefined;
            }

            const localParentType = state.checker.getTypeAtLocation(propertyAccessExpression.expression);
            if (!localParentType) {
                return undefined;
            }

            if (localParentType.symbol && localParentType.symbol.flags & (SymbolFlags.Class | SymbolFlags.Interface) && localParentType.symbol !== search.symbol.parent) {
                return [localParentType.symbol];
            }
            else if (localParentType.flags & TypeFlags.UnionOrIntersection) {
                return getSymbolsForClassAndInterfaceComponents(<UnionOrIntersectionType>localParentType);
            }
        }
    }

    //duplicate code?
    function isExternalModuleSymbol(moduleSymbol: Symbol) { //as opposed to a namespace
        for (const decl of moduleSymbol.declarations) {
            if (decl.kind === SyntaxKind.SourceFile) {
                return true;
            }
            if (decl.kind === SyntaxKind.ModuleDeclaration) {
                return decl.name.kind === SyntaxKind.StringLiteral;
            }
        }
        throw new Error("Should be unreachable");
    }

    //move
    function getExportInfo(exportSymbol: Symbol, exportKind: ExportKind): ExportInfo | undefined {
        const moduleSymbol = exportSymbol.parent;
        //const moduleDecl = getContainingModule(referenceLocation); //Or just symbol.parent....
        //For export in a namespace, just rely on global search
        return isExternalModuleSymbol(moduleSymbol) ? { moduleSymbol, kind: exportKind } : undefined;
    }

    function getReferencesAtLocation(sourceFile: SourceFile, position: number, search: Search, parentSymbols: Symbol[], state: State): void {
        const referenceLocation = getTouchingPropertyName(sourceFile, position);

        if (!isValidReferencePosition(referenceLocation, search.text)) {
            if (!state.implementations) {
                addStringOrCommentReference(sourceFile, position, state, search);
            }
            return;
        }

        if (!(getMeaningFromLocation(referenceLocation) & state.searchMeaning)) {
            return;
        }

        const referenceSymbol = state.checker.getSymbolAtLocation(referenceLocation);
        if (!referenceSymbol) {
            return;
        }

        //might be `export { foo }. In this case, if we are searching for `foo` the symbol won't match, since the symbol at `export { foo }` is an alias to it.`
        //TODO: test `export default foo;`
        //TODO: test for { foo as bar }
        //Actually, could just always skip export specifiers (no `seenExportSpecifier`)??? Handled below.
        //TODO: what about .propertyName?
        if (isExportSpecifier(referenceLocation.parent)) {
            if (referenceLocation.parent.propertyName && referenceLocation.parent.name === referenceLocation) {
                //Don't keep searching, we're at a rename.
                addReferenceToRelatedSymbol(referenceLocation, referenceSymbol);
                return;
            }

            const aliased = state.checker.getShallowTargetOfExportSpecifier(referenceSymbol);
            if (search.includes(aliased)) {
                addReferenceToRelatedSymbol(referenceLocation, aliased); //Choose to make this a reference to the thing it aliases, not a reference to itself.
                findAllRefsGloballyOrExported(state.createSearch(referenceLocation, referenceSymbol), state, getExportInfo(referenceSymbol, ExportKind.Named));
            }

            return;
        }

        const relatedSymbol = getRelatedSymbol(search, referenceSymbol, referenceLocation, parentSymbols, state);
        if (relatedSymbol) {
            if (state.originalLocation.kind === SyntaxKind.ConstructorKeyword) {
                if (isNewExpressionTarget(referenceLocation)) {
                    addReferenceToRelatedSymbol(referenceLocation, relatedSymbol);
                }

                findAdditionalConstructorReferences(referenceSymbol, referenceLocation);
            }
            else {
                addReferenceToRelatedSymbol(referenceLocation, relatedSymbol);
            }

            const { imported, exported } = getImportExportSymbols(referenceLocation, referenceSymbol, state.checker);

            //TODO: For a rename, *don't* traverse through imports *ever*. Want to rename the most local symbol possible.
            //This requires ability for rename to change `import { x }` to `import { x as y }`.
            //For rename, to not continue if it's a default import b/c we can just rename locally.
            //TODO: combine isRenameImport with isEqualsOrDefault...
            if (imported && !(imported.isRenameImport) && !(state.isForRename && imported.isEqualsOrDefault)) { //TODO: state.isForRename && imported.isRenameImport? But that would change lots of tests.
                const searchFile = imported.location.getSourceFile(); //go to the symbol we imported from and find references for it.
                //This is in the source file of the import, so it will lead to finding export references.
                //TODO: could this just call findAllRefsGloballyOrExported?????
                getReferencesInContainer(searchFile, state.createSearch(imported.location, imported.symbol), state);
                //Problem w/ below: we exclude the exporting module from being considered in `findAllRefsGloballyOrExported`. So must search in itself first.
                //findAllRefsGloballyOrExported(state.createSearch(imported.location, imported.symbol), state, getExportKindForSymbol(imported.symbol));
            }
            if (exported !== undefined) {
                //Still look through exports for a rename, because those will be affected too!
                findAllRefsGloballyOrExported(state.createSearch(referenceLocation, exported.symbol), state, exported.info);
            }

            return;
        }

        const referenceSymbolDeclaration = referenceSymbol.valueDeclaration;
        const shorthandValueSymbol = state.checker.getShorthandAssignmentValueSymbol(referenceSymbolDeclaration);
        /* Because in short-hand property assignment, an identifier which stored as name of the short-hand property assignment
            * has two meanings: property name and property value. Therefore when we do findAllReference at the position where
            * an identifier is declared, the language service should return the position of the variable declaration as well as
            * the position in short-hand property assignment excluding property accessing. However, if we do findAllReference at the
            * position of property accessing, the referenceEntry of such position will be handled in the first case.
            */
        if (!(referenceSymbol.flags & SymbolFlags.Transient) && search.includes(shorthandValueSymbol)) {
            addReferenceToRelatedSymbol(referenceSymbolDeclaration.name, shorthandValueSymbol);
        }
        return;

        /** Adds references when a constructor is used with `new this()` in its own class and `super()` calls in subclasses.  */
        function findAdditionalConstructorReferences(referenceSymbol: Symbol, referenceLocation: Node): void {
            Debug.assert(referenceSymbol === search.symbol);

            const classSymbol = skipAliases(search.symbol, state.checker);
            Debug.assert(isClassLike(classSymbol.valueDeclaration));
            const pusher = state.getReferencePusher(search.symbol, search.location);

            if (isClassLike(referenceLocation.parent)) {
                Debug.assert(referenceLocation.parent.name === referenceLocation);
                // This is the class declaration containing the constructor.
                findOwnConstructorCalls(search.symbol, sourceFile, pusher);
            }
            else {
                // If this class appears in `extends C`, then the extending class' "super" calls are references.
                const classExtending = tryGetClassByExtendingIdentifier(referenceLocation);
                if (classExtending && isClassLike(classExtending)) {
                    findSuperConstructorAccesses(classExtending, pusher);
                }
            }
        }

        //review. Does this need to be in a closure????
        function addReferenceToRelatedSymbol(node: Node, relatedSymbol: Symbol) {
            const addReference = state.getReferencePusher(relatedSymbol, search.location);
            if (state.implementations) {
                addImplementationReferences(node, addReference, state); //neater
            }
            else {
                addReference(node);
            }
        }
    }

    //move
    function skipAliases(symbol: Symbol, checker: TypeChecker): Symbol {
        if (symbol.flags & SymbolFlags.Alias) {
            return checker.getAliasedSymbol(symbol);
        }
        return symbol;
    }

    function skipExportSpecifierSymbol(symbol: Symbol, checker: TypeChecker): Symbol {
        if (symbol.declarations.some(isExportSpecifier)) {
            return checker.getShallowTargetOfExportSpecifier(symbol); //move these calls to one place
        }
        return symbol;
    }

    //!
    function getExportNodeFromNodeNodeNode(parent: Node) {
        if (parent.kind === SyntaxKind.VariableDeclaration) {
            return getAncestor(parent, SyntaxKind.VariableStatement);
        }
        return parent;
    }

    //given a match, look for another symbol to search.
    //If we're at an import, look for what it imports.
    //If we're at an export, look for imports of it.
    //exported alrways returns the input symbol, so just return the exportinfo
    function getImportExportSymbols(node: Node, symbol: Symbol, checker: TypeChecker): { imported?: { location: Node, symbol: Symbol, isRenameImport: boolean, isEqualsOrDefault: boolean }, exported?: { symbol: Symbol, info: ExportInfo } } {
        let imported: { location: Node, symbol: Symbol, isRenameImport: boolean, isEqualsOrDefault: boolean } | undefined;
        let exported: { symbol: Symbol, info: ExportInfo } | undefined;
        const { parent } = node;

        function exportInfo(symbol: Symbol) {
            return getExportInfo(symbol, getExportKindForNode(parent));
        }

        if (symbol.flags & SymbolFlags.Export) {//(hasModifier(parent, ModifierFlags.Export)) {
            if (symbol.declarations.some(x => x === parent)) {
                switch (getSpecialPropertyAssignmentKind(parent.parent)) {
                    case SpecialPropertyAssignmentKind.ExportsProperty:
                        return { exported: { symbol, info: getExportInfo(symbol, ExportKind.Named) } };
                    case SpecialPropertyAssignmentKind.ModuleExports:
                        return { exported: { symbol, info: getExportInfo(symbol, ExportKind.ExportEquals) } };
                    case SpecialPropertyAssignmentKind.PrototypeProperty:
                    case SpecialPropertyAssignmentKind.ThisProperty:
                        return {};
                    case SpecialPropertyAssignmentKind.None:
                        //test
                        const { exportSymbol } = symbol;
                        Debug.assert(!!exportSymbol);
                        exported = { symbol: exportSymbol, info: exportInfo(exportSymbol) };
                        break;
                }
                //May be an export-import?
            }
        } else if (hasModifier(getExportNodeFromNodeNodeNode(parent), ModifierFlags.Export)) {
            exported = { symbol, info: exportInfo(symbol) };
        }

        const isImport = nodeIsImport(node);
        if (isImport) {
            //A symbol being imported is always an alias. So get what that aliases to find the local symbol.

            let importedSymbol = checker.getImmediateAliasedSymbol(symbol);

            if (importedSymbol) { //may be a bad reference. Don't crash!
                //Debug.assert(!!(importSymbol.flags & SymbolFlags.Alias));
                //const importSymbol2 = checker.getImmediateAliasedSymbol(importSymbol);

                //importSymbol = importSymbol2;
                importedSymbol = skipExportSpecifierSymbol(importedSymbol, checker); //Don't bother with the export alias.

                //Problem: 'getAliasedSymbol' apparently follows multiple aliases!!! (In 'renameImportOfExportEquals' it went from `import { N } from "test"` to `namespace N`!!!

                //TODO: import { default as foo } is a default import!!!!!
                //TODO: just use `node` instead of `importSymbol.declarations[0]`? Might change "definition" text slightly...
                imported = { symbol: importedSymbol, location: importedSymbol.declarations[0], isRenameImport: symbolName(importedSymbol) !== symbol.name, isEqualsOrDefault: isImport.isEqualsOrDefault };
                //A third kind of import is to "import * as foo". But we don't recursively find "import *" in other modules.
                //Find-all-refs for a module should be done on the module specifier instead.
            }
        }

        //neater: we return `undefined` from `exportInfo` to signal that we should ignore an export
        if (exported && !exported.info) {
            exported = undefined;
        }

        return { imported, exported };
    }

    //move
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

    //neater
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

            default:
                return undefined;
        }
    }

    function addStringOrCommentReference(sourceFile: SourceFile, position: number, state: State, search: Search) {
        // This wasn't the start of a token.  Check to see if it might be a
        // match in a comment or string if that's what the caller is asking
        // for.
        if (state.findInStrings && isInString(sourceFile, position) || state.findInComments && isInNonReferenceComment(sourceFile, position)) {
            // In the case where we're looking inside comments/strings, we don't have
            // an actual definition.  So just use 'undefined' here.  Features like
            // 'Rename' won't care (as they ignore the definitions), and features like
            // 'FindReferences' will just filter out these results.
            state.addStringOrCommentReference(sourceFile.fileName, createTextSpan(position, search.text.length))
        }
    }

    function getPropertyAccessExpressionFromRightHandSide(node: Node): PropertyAccessExpression {
        return isRightSideOfPropertyAccess(node) && <PropertyAccessExpression>node.parent;
    }

    /** `classSymbol` is the class where the constructor was defined.
     * Reference the constructor and all calls to `new this()`.
     */ //also finds declarations. findOwnConstructorCallsAndDeclarations
    function findOwnConstructorCalls(classSymbol: Symbol, sourceFile: SourceFile, addNode: (node: Node) => void): void {
        for (const decl of classSymbol.members.get("__constructor").declarations) {
            const ctrKeyword = ts.findChildOfKind(decl, ts.SyntaxKind.ConstructorKeyword, sourceFile)!
            Debug.assert(decl.kind === SyntaxKind.Constructor && !!ctrKeyword);
            addNode(ctrKeyword);
        }

        classSymbol.exports.forEach(member => {
            const decl = member.valueDeclaration;
            if (decl && decl.kind === SyntaxKind.MethodDeclaration) {
                const body = (<MethodDeclaration>decl).body;
                if (body) {
                    forEachDescendantOfKind(body, SyntaxKind.ThisKeyword, thisKeyword => {
                        if (isNewExpressionTarget(thisKeyword)) {
                            addNode(thisKeyword);
                        }
                    });
                }
            }
        });
    }

    /** Find references to `super` in the constructor of an extending class.  */
    function findSuperConstructorAccesses(cls: ClassLikeDeclaration, addNode: (node: Node) => void): void {
        const symbol = cls.symbol;
        const ctr = symbol.members.get("__constructor");
        if (!ctr) {
            return;
        }

        for (const decl of ctr.declarations) {
            Debug.assert(decl.kind === SyntaxKind.Constructor);
            const body = (<ConstructorDeclaration>decl).body;
            if (body) {
                forEachDescendantOfKind(body, SyntaxKind.SuperKeyword, node => {
                    if (isCallExpressionTarget(node)) {
                        addNode(node);
                    }
                });
            }
        };
    }

    function addImplementationReferences(refNode: Node, addReference: (node: Node) => void, state: State): void {
        // Check if we found a function/propertyAssignment/method with an implementation or initializer
        if (isDeclarationName(refNode) && isImplementation(refNode.parent)) {
            addReference(refNode.parent);
            return;
        }

        if (refNode.kind !== SyntaxKind.Identifier) {
            return;
        }

        if (refNode.parent.kind === SyntaxKind.ShorthandPropertyAssignment) {
            // Go ahead and dereference the shorthand assignment by going to its definition
            getReferenceEntriesForShorthandPropertyAssignment(refNode, state.checker, addReference);
        }

        // Check if the node is within an extends or implements clause
        const containingClass = getContainingClassIfInHeritageClause(refNode);
        if (containingClass) {
            addReference(containingClass);
            return;
        }

        // If we got a type reference, try and see if the reference applies to any expressions that can implement an interface
        const containingTypeReference = getContainingTypeReference(refNode);
        if (containingTypeReference && !state.markSeenContainingTypeReference(containingTypeReference)) {
            const parent = containingTypeReference.parent;
            if (isVariableLike(parent) && parent.type === containingTypeReference && parent.initializer && isImplementationExpression(parent.initializer)) {
                addReference(parent.initializer);
            }
            else if (isFunctionLike(parent) && parent.type === containingTypeReference && parent.body) {
                if (parent.body.kind === SyntaxKind.Block) {
                    forEachReturnStatement(<Block>parent.body, returnStatement => {
                        if (returnStatement.expression && isImplementationExpression(returnStatement.expression)) {
                            addReference(returnStatement.expression);
                        }
                    });
                }
                else if (isImplementationExpression(<Expression>parent.body)) {
                    addReference(parent.body);
                }
            }
            else if (isAssertionExpression(parent) && isImplementationExpression(parent.expression)) {
                addReference(parent.expression);
            }
        }
    }

    function getSymbolsForClassAndInterfaceComponents(type: UnionOrIntersectionType, result: Symbol[] = []): Symbol[] {
        for (const componentType of type.types) {
            if (componentType.symbol && componentType.symbol.getFlags() & (SymbolFlags.Class | SymbolFlags.Interface)) {
                result.push(componentType.symbol);
            }
            if (componentType.getFlags() & TypeFlags.UnionOrIntersection) {
                getSymbolsForClassAndInterfaceComponents(<UnionOrIntersectionType>componentType, result);
            }
        }
        return result;
    }

    function getContainingTypeReference(node: Node): Node {
        let topLevelTypeReference: Node = undefined;

        while (node) {
            if (isTypeNode(node)) {
                topLevelTypeReference = node;
            }
            node = node.parent;
        }

        return topLevelTypeReference;
    }

    function getContainingClassIfInHeritageClause(node: Node): ClassLikeDeclaration {
        if (node && node.parent) {
            if (node.kind === SyntaxKind.ExpressionWithTypeArguments
                && node.parent.kind === SyntaxKind.HeritageClause
                && isClassLike(node.parent.parent)) {
                return node.parent.parent;
            }

            else if (node.kind === SyntaxKind.Identifier || node.kind === SyntaxKind.PropertyAccessExpression) {
                return getContainingClassIfInHeritageClause(node.parent);
            }
        }
        return undefined;
    }

    /**
     * Returns true if this is an expression that can be considered an implementation
     */
    function isImplementationExpression(node: Expression): boolean {
        switch (node.kind) {
            case SyntaxKind.ParenthesizedExpression:
                return isImplementationExpression((<ParenthesizedExpression>node).expression);
            case SyntaxKind.ArrowFunction:
            case SyntaxKind.FunctionExpression:
            case SyntaxKind.ObjectLiteralExpression:
            case SyntaxKind.ClassExpression:
            case SyntaxKind.ArrayLiteralExpression:
                return true;
            default:
                return false;
        }
    }

    /**
     * Determines if the parent symbol occurs somewhere in the child's ancestry. If the parent symbol
     * is an interface, determines if some ancestor of the child symbol extends or inherits from it.
     * Also takes in a cache of previous results which makes this slightly more efficient and is
     * necessary to avoid potential loops like so:
     *     class A extends B { }
     *     class B extends A { }
     *
     * We traverse the AST rather than using the type checker because users are typically only interested
     * in explicit implementations of an interface/class when calling "Go to Implementation". Sibling
     * implementations of types that share a common ancestor with the type whose implementation we are
     * searching for need to be filtered out of the results. The type checker doesn't let us make the
     * distinction between structurally compatible implementations and explicit implementations, so we
     * must use the AST.
     *
     * @param child         A class or interface Symbol
     * @param parent        Another class or interface Symbol
     * @param cachedResults A map of symbol id pairs (i.e. "child,parent") to booleans indicating previous results
     */
    function explicitlyInheritsFrom(child: Symbol, parent: Symbol, cachedResults: Map<boolean>, checker: TypeChecker): boolean {
        const parentIsInterface = parent.getFlags() & SymbolFlags.Interface;
        return searchHierarchy(child);

        function searchHierarchy(symbol: Symbol): boolean {
            if (symbol === parent) {
                return true;
            }

            const key = getSymbolId(symbol) + "," + getSymbolId(parent);
            const cached = cachedResults.get(key);
            if (cached !== undefined) {
                return cached;
            }

            // Set the key so that we don't infinitely recurse
            cachedResults.set(key, false);

            const inherits = forEach(symbol.getDeclarations(), declaration => {
                if (isClassLike(declaration)) {
                    if (parentIsInterface) {
                        const interfaceReferences = getClassImplementsHeritageClauseElements(declaration);
                        if (interfaceReferences) {
                            for (const typeReference of interfaceReferences) {
                                if (searchTypeReference(typeReference)) {
                                    return true;
                                }
                            }
                        }
                    }
                    return searchTypeReference(getClassExtendsHeritageClauseElement(declaration));
                }
                else if (declaration.kind === SyntaxKind.InterfaceDeclaration) {
                    if (parentIsInterface) {
                        return forEach(getInterfaceBaseTypeNodes(<InterfaceDeclaration>declaration), searchTypeReference);
                    }
                }
                return false;
            });

            cachedResults.set(key, inherits);
            return inherits;
        }

        function searchTypeReference(typeReference: ExpressionWithTypeArguments): boolean {
            if (typeReference) {
                const type = checker.getTypeAtLocation(typeReference);
                if (type && type.symbol) {
                    return searchHierarchy(type.symbol);
                }
            }
            return false;
        }
    }

    function getReferencesForSuperKeyword(superKeyword: Node, checker: TypeChecker, cancellationToken: CancellationToken): ReferencedSymbol[] {
        let searchSpaceNode = getSuperContainer(superKeyword, /*stopOnFunctions*/ false);
        if (!searchSpaceNode) {
            return undefined;
        }
        // Whether 'super' occurs in a static context within a class.
        let staticFlag = ModifierFlags.Static;

        switch (searchSpaceNode.kind) {
            case SyntaxKind.PropertyDeclaration:
            case SyntaxKind.PropertySignature:
            case SyntaxKind.MethodDeclaration:
            case SyntaxKind.MethodSignature:
            case SyntaxKind.Constructor:
            case SyntaxKind.GetAccessor:
            case SyntaxKind.SetAccessor:
                staticFlag &= getModifierFlags(searchSpaceNode);
                searchSpaceNode = searchSpaceNode.parent; // re-assign to be the owning class
                break;
            default:
                return undefined;
        }

        const references: ReferenceEntry[] = [];

        const sourceFile = searchSpaceNode.getSourceFile();
        const possiblePositions = getPossibleSymbolReferencePositions(sourceFile, "super", searchSpaceNode.getStart(), searchSpaceNode.getEnd(), cancellationToken);
        for (const position of possiblePositions) {
            cancellationToken.throwIfCancellationRequested();

            const node = getTouchingWord(sourceFile, position);

            if (!node || node.kind !== SyntaxKind.SuperKeyword) {
                continue;
            }

            const container = getSuperContainer(node, /*stopOnFunctions*/ false);

            // If we have a 'super' container, we must have an enclosing class.
            // Now make sure the owning class is the same as the search-space
            // and has the same static qualifier as the original 'super's owner.
            if (container && (ModifierFlags.Static & getModifierFlags(container)) === staticFlag && container.parent.symbol === searchSpaceNode.symbol) {
                references.push(getReferenceEntryFromNode(node));
            }
        }

        const definition = getDefinition(searchSpaceNode.symbol, superKeyword, checker);
        return [{ definition, references }];
    }

    function getReferencesForThisKeyword(thisOrSuperKeyword: Node, sourceFiles: SourceFile[], checker: TypeChecker, cancellationToken: CancellationToken): ReferencedSymbol[] {
        let searchSpaceNode = getThisContainer(thisOrSuperKeyword, /* includeArrowFunctions */ false);

        // Whether 'this' occurs in a static context within a class.
        let staticFlag = ModifierFlags.Static;

        switch (searchSpaceNode.kind) {
            case SyntaxKind.MethodDeclaration:
            case SyntaxKind.MethodSignature:
                if (isObjectLiteralMethod(searchSpaceNode)) {
                    break;
                }
            // fall through
            case SyntaxKind.PropertyDeclaration:
            case SyntaxKind.PropertySignature:
            case SyntaxKind.Constructor:
            case SyntaxKind.GetAccessor:
            case SyntaxKind.SetAccessor:
                staticFlag &= getModifierFlags(searchSpaceNode);
                searchSpaceNode = searchSpaceNode.parent; // re-assign to be the owning class
                break;
            case SyntaxKind.SourceFile:
                if (isExternalModule(<SourceFile>searchSpaceNode)) {
                    return undefined;
                }
            // Fall through
            case SyntaxKind.FunctionDeclaration:
            case SyntaxKind.FunctionExpression:
                break;
            // Computed properties in classes are not handled here because references to this are illegal,
            // so there is no point finding references to them.
            default:
                return undefined;
        }

        const references: ReferenceEntry[] = [];

        let possiblePositions: number[];
        if (searchSpaceNode.kind === SyntaxKind.SourceFile) {
            forEach(sourceFiles, sourceFile => {
                possiblePositions = getPossibleSymbolReferencePositions(sourceFile, "this", sourceFile.getStart(), sourceFile.getEnd(), cancellationToken);
                getThisReferencesInFile(sourceFile, sourceFile, possiblePositions, references);
            });
        }
        else {
            const sourceFile = searchSpaceNode.getSourceFile();
            possiblePositions = getPossibleSymbolReferencePositions(sourceFile, "this", searchSpaceNode.getStart(), searchSpaceNode.getEnd(), cancellationToken);
            getThisReferencesInFile(sourceFile, searchSpaceNode, possiblePositions, references);
        }

        const thisOrSuperSymbol = checker.getSymbolAtLocation(thisOrSuperKeyword);

        const displayParts = thisOrSuperSymbol && SymbolDisplay.getSymbolDisplayPartsDocumentationAndSymbolKind(
            checker, thisOrSuperSymbol, thisOrSuperKeyword.getSourceFile(), getContainerNode(thisOrSuperKeyword), thisOrSuperKeyword).displayParts;

        return [{
            definition: {
                containerKind: "",
                containerName: "",
                fileName: thisOrSuperKeyword.getSourceFile().fileName,
                kind: ScriptElementKind.variableElement,
                name: "this",
                textSpan: createTextSpanFromNode(thisOrSuperKeyword),
                displayParts
            },
            references: references
        }];

        function getThisReferencesInFile(sourceFile: SourceFile, searchSpaceNode: Node, possiblePositions: number[], result: ReferenceEntry[]): void {
            forEach(possiblePositions, position => {
                cancellationToken.throwIfCancellationRequested();

                const node = getTouchingWord(sourceFile, position);
                if (!node || !isThis(node)) {
                    return;
                }

                const container = getThisContainer(node, /* includeArrowFunctions */ false);

                switch (searchSpaceNode.kind) {
                    case SyntaxKind.FunctionExpression:
                    case SyntaxKind.FunctionDeclaration:
                        if (searchSpaceNode.symbol === container.symbol) {
                            result.push(getReferenceEntryFromNode(node));
                        }
                        break;
                    case SyntaxKind.MethodDeclaration:
                    case SyntaxKind.MethodSignature:
                        if (isObjectLiteralMethod(searchSpaceNode) && searchSpaceNode.symbol === container.symbol) {
                            result.push(getReferenceEntryFromNode(node));
                        }
                        break;
                    case SyntaxKind.ClassExpression:
                    case SyntaxKind.ClassDeclaration:
                        // Make sure the container belongs to the same class
                        // and has the appropriate static modifier from the original container.
                        if (container.parent && searchSpaceNode.symbol === container.parent.symbol && (getModifierFlags(container) & ModifierFlags.Static) === staticFlag) {
                            result.push(getReferenceEntryFromNode(node));
                        }
                        break;
                    case SyntaxKind.SourceFile:
                        if (container.kind === SyntaxKind.SourceFile && !isExternalModule(<SourceFile>container)) {
                            result.push(getReferenceEntryFromNode(node));
                        }
                        break;
                }
            });
        }
    }

    function getReferencesForStringLiteral(node: StringLiteral, sourceFiles: SourceFile[], checker: TypeChecker, cancellationToken: CancellationToken): ReferencedSymbol[] {
        const type = getStringLiteralTypeForNode(node, checker);

        if (!type) {
            // nothing to do here. moving on
            return undefined;
        }

        const references: ReferenceEntry[] = [];

        for (const sourceFile of sourceFiles) {
            const possiblePositions = getPossibleSymbolReferencePositions(sourceFile, type.text, sourceFile.getStart(), sourceFile.getEnd(), cancellationToken);
            getReferencesForStringLiteralInFile(sourceFile, type, possiblePositions, references);
        }

        return [{
            definition: {
                containerKind: "",
                containerName: "",
                fileName: node.getSourceFile().fileName,
                kind: ScriptElementKind.variableElement,
                name: type.text,
                textSpan: createTextSpanFromNode(node),
                displayParts: [displayPart(getTextOfNode(node), SymbolDisplayPartKind.stringLiteral)]
            },
            references: references
        }];

        function getReferencesForStringLiteralInFile(sourceFile: SourceFile, searchType: Type, possiblePositions: number[], references: ReferenceEntry[]): void {
            for (const position of possiblePositions) {
                cancellationToken.throwIfCancellationRequested();

                const node = getTouchingWord(sourceFile, position);
                if (!node || node.kind !== SyntaxKind.StringLiteral) {
                    return;
                }

                const type = getStringLiteralTypeForNode(<StringLiteral>node, checker);
                if (type === searchType) {
                    references.push(getReferenceEntryFromNode(node));
                }
            }
        }
    }

    //yes, still need this
    function populateSearchSymbolSet(symbol: Symbol, location: Node, checker: TypeChecker, implementations: boolean): Symbol[] {
        // The search set contains at least the current symbol
        const result = [symbol];

        const containingObjectLiteralElement = getContainingObjectLiteralElement(location);
        if (containingObjectLiteralElement) {
            // If the location is name of property symbol from object literal destructuring pattern
            // Search the property symbol
            //      for ( { property: p2 } of elems) { }
            if (containingObjectLiteralElement.kind !== SyntaxKind.ShorthandPropertyAssignment) {
                const propertySymbol = getPropertySymbolOfDestructuringAssignment(location, checker);
                if (propertySymbol) {
                    result.push(propertySymbol);
                }
            }

            // If the location is in a context sensitive location (i.e. in an object literal) try
            // to get a contextual type for it, and add the property symbol from the contextual
            // type to the search set
            forEach(getPropertySymbolsFromContextualType(containingObjectLiteralElement, checker), contextualSymbol => {
                addRange(result, checker.getRootSymbols(contextualSymbol));
            });

            /* Because in short-hand property assignment, location has two meaning : property name and as value of the property
             * When we do findAllReference at the position of the short-hand property assignment, we would want to have references to position of
             * property name and variable declaration of the identifier.
             * Like in below example, when querying for all references for an identifier 'name', of the property assignment, the language service
             * should show both 'name' in 'obj' and 'name' in variable declaration
             *      const name = "Foo";
             *      const obj = { name };
             * In order to do that, we will populate the search set with the value symbol of the identifier as a value of the property assignment
             * so that when matching with potential reference symbol, both symbols from property declaration and variable declaration
             * will be included correctly.
             */
            const shorthandValueSymbol = checker.getShorthandAssignmentValueSymbol(location.parent);
            if (shorthandValueSymbol) {
                result.push(shorthandValueSymbol);
            }
        }

        // If the symbol.valueDeclaration is a property parameter declaration,
        // we should include both parameter declaration symbol and property declaration symbol
        // Parameter Declaration symbol is only visible within function scope, so the symbol is stored in constructor.locals.
        // Property Declaration symbol is a member of the class, so the symbol is stored in its class Declaration.symbol.members
        if (symbol.valueDeclaration && symbol.valueDeclaration.kind === SyntaxKind.Parameter &&
            isParameterPropertyDeclaration(<ParameterDeclaration>symbol.valueDeclaration)) {
            addRange(result, checker.getSymbolsOfParameterPropertyDeclaration(<ParameterDeclaration>symbol.valueDeclaration, symbol.name));
        }

        // If this is symbol of binding element without propertyName declaration in Object binding pattern
        // Include the property in the search
        const bindingElementPropertySymbol = getPropertySymbolOfObjectBindingPatternWithoutPropertyName(symbol, checker);
        if (bindingElementPropertySymbol) {
            result.push(bindingElementPropertySymbol);
        }

        // If this is a union property, add all the symbols from all its source symbols in all unioned types.
        // If the symbol is an instantiation from a another symbol (e.g. widened symbol) , add the root the list
        for (const rootSymbol of checker.getRootSymbols(symbol)) {
            if (rootSymbol !== symbol) {
                result.push(rootSymbol);
            }

            // Add symbol of properties/methods of the same name in base classes and implemented interfaces definitions
            if (!implementations && rootSymbol.parent && rootSymbol.parent.flags & (SymbolFlags.Class | SymbolFlags.Interface)) {
                getPropertySymbolsFromBaseTypes(rootSymbol.parent, rootSymbol.getName(), result, /*previousIterationSymbolsCache*/ createMap<Symbol>(), checker);
            }
        }

        return result;
    }

    /**
     * Find symbol of the given property-name and add the symbol to the given result array
     * @param symbol a symbol to start searching for the given propertyName
     * @param propertyName a name of property to search for
     * @param result an array of symbol of found property symbols
     * @param previousIterationSymbolsCache a cache of symbol from previous iterations of calling this function to prevent infinite revisiting of the same symbol.
     *                                The value of previousIterationSymbol is undefined when the function is first called.
     */
    function getPropertySymbolsFromBaseTypes(symbol: Symbol, propertyName: string, result: Symbol[], previousIterationSymbolsCache: SymbolTable, checker: TypeChecker): void {
        if (!symbol) {
            return;
        }

        // If the current symbol is the same as the previous-iteration symbol, we can just return the symbol that has already been visited
        // This is particularly important for the following cases, so that we do not infinitely visit the same symbol.
        // For example:
        //      interface C extends C {
        //          /*findRef*/propName: string;
        //      }
        // The first time getPropertySymbolsFromBaseTypes is called when finding-all-references at propName,
        // the symbol argument will be the symbol of an interface "C" and previousIterationSymbol is undefined,
        // the function will add any found symbol of the property-name, then its sub-routine will call
        // getPropertySymbolsFromBaseTypes again to walk up any base types to prevent revisiting already
        // visited symbol, interface "C", the sub-routine will pass the current symbol as previousIterationSymbol.
        if (previousIterationSymbolsCache.has(symbol.name)) {
            return;
        }

        if (symbol.flags & (SymbolFlags.Class | SymbolFlags.Interface)) {
            forEach(symbol.getDeclarations(), declaration => {
                if (isClassLike(declaration)) {
                    getPropertySymbolFromTypeReference(getClassExtendsHeritageClauseElement(<ClassDeclaration>declaration));
                    forEach(getClassImplementsHeritageClauseElements(<ClassDeclaration>declaration), getPropertySymbolFromTypeReference);
                }
                else if (declaration.kind === SyntaxKind.InterfaceDeclaration) {
                    forEach(getInterfaceBaseTypeNodes(<InterfaceDeclaration>declaration), getPropertySymbolFromTypeReference);
                }
            });
        }
        return;

        function getPropertySymbolFromTypeReference(typeReference: ExpressionWithTypeArguments) {
            if (typeReference) {
                const type = checker.getTypeAtLocation(typeReference);
                if (type) {
                    const propertySymbol = checker.getPropertyOfType(type, propertyName);
                    if (propertySymbol) {
                        result.push(...checker.getRootSymbols(propertySymbol));
                    }

                    // Visit the typeReference as well to see if it directly or indirectly use that property
                    previousIterationSymbolsCache.set(symbol.name, symbol);
                    getPropertySymbolsFromBaseTypes(type.symbol, propertyName, result, previousIterationSymbolsCache, checker);
                }
            }
        }
    }

    //Needs a better name!!!
    function getRelatedSymbol(search: Search, referenceSymbol: Symbol, referenceLocation: Node, parents: Symbol[] | undefined, state: State): Symbol | undefined {
        if (search.includes(referenceSymbol)) { //use search.includes
            // If we are searching for constructor uses, they must be 'new' expressions.
            return referenceSymbol;
        }

        // If the reference symbol is an alias, check if what it is aliasing is one of the search
        // symbols but by looking up for related symbol of this alias so it can handle multiple level of indirectness.
        //TODO: shouldn't need to do this!
        //const aliasSymbol = getAliasSymbolForPropertyNameSymbol(referenceSymbol, referenceLocation, state.checker);
        //if (aliasSymbol) {
        //    return getRelatedSymbol(searchSymbols, aliasSymbol, referenceLocation, searchLocationIsConstructor, parents, state);
        //}

        // If the reference location is in an object literal, try to get the contextual type for the
        // object literal, lookup the property symbol in the contextual type, and use this symbol to
        // compare to our searchSymbol
        const containingObjectLiteralElement = getContainingObjectLiteralElement(referenceLocation);
        if (containingObjectLiteralElement) {
            const contextualSymbol = forEach(getPropertySymbolsFromContextualType(containingObjectLiteralElement, state.checker), contextualSymbol =>
                find(state.checker.getRootSymbols(contextualSymbol), search.includes));

            if (contextualSymbol) {
                return contextualSymbol;
            }

            // If the reference location is the name of property from object literal destructuring pattern
            // Get the property symbol from the object literal's type and look if thats the search symbol
            // In below eg. get 'property' from type of elems iterating type
            //      for ( { property: p2 } of elems) { }
            const propertySymbol = getPropertySymbolOfDestructuringAssignment(referenceLocation, state.checker);
            if (propertySymbol && search.includes(propertySymbol)) {
                return propertySymbol;
            }
        }

        // If the reference location is the binding element and doesn't have property name
        // then include the binding element in the related symbols
        //      let { a } : { a };
        const bindingElementPropertySymbol = getPropertySymbolOfObjectBindingPatternWithoutPropertyName(referenceSymbol, state.checker);
        if (bindingElementPropertySymbol && search.includes(bindingElementPropertySymbol)) {
            return bindingElementPropertySymbol;
        }

        // Unwrap symbols to get to the root (e.g. transient symbols as a result of widening)
        // Or a union property, use its underlying unioned symbols
        return forEach(state.checker.getRootSymbols(referenceSymbol), rootSymbol => {
            // if it is in the list, then we are done
            if (search.includes(rootSymbol)) {
                return rootSymbol;
            }

            // Finally, try all properties with the same name in any type the containing type extended or implemented, and
            // see if any is in the list. If we were passed a parent symbol, only include types that are subtypes of the
            // parent symbol
            if (rootSymbol.parent && rootSymbol.parent.flags & (SymbolFlags.Class | SymbolFlags.Interface)) {
                // Parents will only be defined if implementations is true
                if (parents && !some(parents, parent => explicitlyInheritsFrom(rootSymbol.parent, parent, state.inheritsFromCache, state.checker))) {
                    return undefined;
                }

                const result: Symbol[] = [];
                getPropertySymbolsFromBaseTypes(rootSymbol.parent, rootSymbol.getName(), result, /*previousIterationSymbolsCache*/ createMap<Symbol>(), state.checker);
                return find(result, search.includes);
            }

            return undefined;
        });
    }

    function getNameFromObjectLiteralElement(node: ObjectLiteralElement) {
        if (node.name.kind === SyntaxKind.ComputedPropertyName) {
            const nameExpression = (<ComputedPropertyName>node.name).expression;
            // treat computed property names where expression is string/numeric literal as just string/numeric literal
            if (isStringOrNumericLiteral(nameExpression)) {
                return (<LiteralExpression>nameExpression).text;
            }
            return undefined;
        }
        return (<Identifier | LiteralExpression>node.name).text;
    }

    /** Gets all symbols for one property. Does not get symbols for every property. */
    function getPropertySymbolsFromContextualType(node: ObjectLiteralElement, checker: TypeChecker): Symbol[] | undefined {
        const objectLiteral = <ObjectLiteralExpression>node.parent;
        const contextualType = checker.getContextualType(objectLiteral);
        const name = getNameFromObjectLiteralElement(node);
        if (name && contextualType) {
            const result: Symbol[] = [];
            const symbol = contextualType.getProperty(name);
            if (symbol) {
                result.push(symbol);
            }

            if (contextualType.flags & TypeFlags.Union) {
                forEach((<UnionType>contextualType).types, t => {
                    const symbol = t.getProperty(name);
                    if (symbol) {
                        result.push(symbol);
                    }
                });
            }
            return result;
        }
        return undefined;
    }

    /**
     * Given an initial searchMeaning, extracted from a location, widen the search scope based on the declarations
     * of the corresponding symbol. e.g. if we are searching for "Foo" in value position, but "Foo" references a class
     * then we need to widen the search to include type positions as well.
     * On the contrary, if we are searching for "Bar" in type position and we trace bar to an interface, and an uninstantiated
     * module, we want to keep the search limited to only types, as the two declarations (interface and uninstantiated module)
     * do not intersect in any of the three spaces.
     */
    function getIntersectingMeaningFromDeclarations(meaning: SemanticMeaning, declarations: Declaration[]): SemanticMeaning {
        if (declarations) {
            let lastIterationMeaning: SemanticMeaning;
            do {
                // The result is order-sensitive, for instance if initialMeaning === Namespace, and declarations = [class, instantiated module]
                // we need to consider both as they initialMeaning intersects with the module in the namespace space, and the module
                // intersects with the class in the value space.
                // To achieve that we will keep iterating until the result stabilizes.

                // Remember the last meaning
                lastIterationMeaning = meaning;

                for (const declaration of declarations) {
                    const declarationMeaning = getMeaningFromDeclaration(declaration);

                    if (declarationMeaning & meaning) {
                        meaning |= declarationMeaning;
                    }
                }
            }
            while (meaning !== lastIterationMeaning);
        }
        return meaning;
    }

    function isImplementation(node: Node): boolean {
        if (!node) {
            return false;
        }
        else if (isVariableLike(node)) {
            if (node.initializer) {
                return true;
            }
            else if (node.kind === SyntaxKind.VariableDeclaration) {
                const parentStatement = getParentStatementOfVariableDeclaration(<VariableDeclaration>node);
                return parentStatement && hasModifier(parentStatement, ModifierFlags.Ambient);
            }
        }
        else if (isFunctionLike(node)) {
            return !!node.body || hasModifier(node, ModifierFlags.Ambient);
        }
        else {
            switch (node.kind) {
                case SyntaxKind.ClassDeclaration:
                case SyntaxKind.ClassExpression:
                case SyntaxKind.EnumDeclaration:
                case SyntaxKind.ModuleDeclaration:
                    return true;
            }
        }
        return false;
    }

    function getParentStatementOfVariableDeclaration(node: VariableDeclaration): VariableStatement {
        if (node.parent && node.parent.parent && node.parent.parent.kind === SyntaxKind.VariableStatement) {
            Debug.assert(node.parent.kind === SyntaxKind.VariableDeclarationList);
            return <VariableStatement>node.parent.parent;
        }
    }

    function getReferenceEntriesForShorthandPropertyAssignment(node: Node, checker: TypeChecker, addReference: (node: Node) => void): void {
        const refSymbol = checker.getSymbolAtLocation(node);
        const shorthandSymbol = checker.getShorthandAssignmentValueSymbol(refSymbol.valueDeclaration);

        if (shorthandSymbol) {
            for (const declaration of shorthandSymbol.getDeclarations()) {
                if (getMeaningFromDeclaration(declaration) & SemanticMeaning.Value) {
                    addReference(declaration);
                }
            }
        }
    }

    function getReferenceEntryFromNode(node: Node): ReferenceEntry {
        return {
            fileName: node.getSourceFile().fileName,
            textSpan: getTextSpan(node),
            isWriteAccess: isWriteAccess(node),
            isDefinition: isDeclarationName(node) || isLiteralComputedPropertyDeclarationName(node)
        };
    }

    function getTextSpan(node: Node) {
        let start = node.getStart();
        let end = node.getEnd();
        if (node.kind === SyntaxKind.StringLiteral) {
            start += 1;
            end -= 1;
        }
        return createTextSpanFromBounds(start, end);
    }

    /** A node is considered a writeAccess iff it is a name of a declaration or a target of an assignment */
    function isWriteAccess(node: Node): boolean {
        if (node.kind === SyntaxKind.Identifier && isDeclarationName(node)) {
            return true;
        }

        const parent = node.parent;
        if (parent) {
            if (parent.kind === SyntaxKind.PostfixUnaryExpression || parent.kind === SyntaxKind.PrefixUnaryExpression) {
                return true;
            }
            else if (parent.kind === SyntaxKind.BinaryExpression && (<BinaryExpression>parent).left === node) {
                const operator = (<BinaryExpression>parent).operatorToken.kind;
                return SyntaxKind.FirstAssignment <= operator && operator <= SyntaxKind.LastAssignment;
            }
        }

        return false;
    }

    function forEachDescendantOfKind(node: Node, kind: SyntaxKind, action: (node: Node) => void) {
        forEachChild(node, child => {
            if (child.kind === kind) {
                action(child);
            }
            forEachDescendantOfKind(child, kind, action);
        });
    }

    /**
     * Returns the containing object literal property declaration given a possible name node, e.g. "a" in x = { "a": 1 }
     */
    function getContainingObjectLiteralElement(node: Node): ObjectLiteralElement {
        switch (node.kind) {
            case SyntaxKind.StringLiteral:
            case SyntaxKind.NumericLiteral:
                if (node.parent.kind === SyntaxKind.ComputedPropertyName) {
                    return isObjectLiteralPropertyDeclaration(node.parent.parent) ? node.parent.parent : undefined;
                }
            // intential fall through
            case SyntaxKind.Identifier:
                return isObjectLiteralPropertyDeclaration(node.parent) && node.parent.name === node ? node.parent : undefined;
        }
        return undefined;
    }

    function isObjectLiteralPropertyDeclaration(node: Node): node is ObjectLiteralElement  {
        switch (node.kind) {
            case SyntaxKind.PropertyAssignment:
            case SyntaxKind.ShorthandPropertyAssignment:
            case SyntaxKind.MethodDeclaration:
            case SyntaxKind.GetAccessor:
            case SyntaxKind.SetAccessor:
                return true;
        }
        return false;
    }

    /** Get `C` given `N` if `N` is in the position `class C extends N` or `class C extends foo.N` where `N` is an identifier. */
    function tryGetClassByExtendingIdentifier(node: Node): ClassLikeDeclaration | undefined {
        return tryGetClassExtendingExpressionWithTypeArguments(climbPastPropertyAccess(node).parent);
    }

    function isNameOfExternalModuleImportOrDeclaration(node: Node): boolean {
        if (node.kind === SyntaxKind.StringLiteral) {
            return isNameOfModuleDeclaration(node) || isExpressionOfExternalModuleImportEqualsDeclaration(node);
        }

        return false;
    }
}
