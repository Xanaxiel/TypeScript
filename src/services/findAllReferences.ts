﻿/// <reference path="./importTracker.ts" />

/* @internal */
namespace ts.FindAllReferences {
    export interface Options {
        readonly findInStrings?: boolean;
        readonly findInComments?: boolean;
        /**
         * True if we are renaming the symbol.
         * If so, we will find fewer references -- if it is referenced by several different names, we sill only find references for the original name.
         */
        readonly isForRename?: boolean;
        /** True if we are searching for implementations. We will have a different method of adding references if so. */
        readonly implementations?: boolean;
    }

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
            getReferenceEntriesForShorthandPropertyAssignment(node, typeChecker, node => result.push(getReferenceEntryFromNode(node)));
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

    export function findReferencedEntries(checker: TypeChecker, cancellationToken: CancellationToken, sourceFiles: SourceFile[], sourceFile: SourceFile, position: number, options?: Options): ReferenceEntry[] | undefined {
        return flattenEntries(findAllReferencedSymbols(checker, cancellationToken, sourceFiles, sourceFile, position, options));
    }

    function findAllReferencedSymbols(checker: TypeChecker, cancellationToken: CancellationToken, sourceFiles: SourceFile[], sourceFile: SourceFile, position: number, options?: Options): ReferencedSymbol[] | undefined {
        const node = getTouchingPropertyName(sourceFile, position, /*includeJsDocComment*/ true);
        return getReferencedSymbolsForNode(node, sourceFiles, checker, cancellationToken, options);
    }

    export function getReferenceEntriesForNode(node: Node, sourceFiles: SourceFile[], checker: TypeChecker, cancellationToken: CancellationToken, options: Options = {}): ReferenceEntry[] | undefined {
        return flattenEntries(getReferencedSymbolsForNode(node, sourceFiles, checker, cancellationToken, options));
    }

    function flattenEntries(referenceSymbols: ReferencedSymbol[]): ReferenceEntry[] {
        return referenceSymbols && flatMap(referenceSymbols, r => r.references);
    }

    /** Core find-all-references algorithm. Handles special cases before delegating to `getReferencedSymbolsForSymbol`. */
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

        const symbol = checker.getSymbolAtLocation(node);

        // Could not find a symbol e.g. unknown identifier
        if (!symbol) {
            // String literal might be a property (and thus have a symbol), so do this here rather than in getReferencedSymbolsSpecial.
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

        return getReferencedSymbolsForSymbol(symbol, node, sourceFiles, checker, cancellationToken, options);
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

    /**
     * Symbol that is currently being searched for.
     * This will be replaced if we find an alias for the symbol.
     */
    interface Search {
        /** If coming from an export, we will not recursively search for the imported symbol (since that's where we came from). */
        readonly comingFrom?: ImportExport;

        readonly location: Node;
        readonly symbol: Symbol;
        readonly text: string;
        readonly escapedText: string;
        /** Only set if `options.implementations` is true. These are the symbols checked to get the implementations of a property access. */
        readonly parents: Symbol[] | undefined;

        /**
         * Whether a symbol is in the search set.
         * Do not compare directly to `symbol` because there may be related symbols to search for. See `populateSearchSymbolSet`.
         */
        includes(symbol: Symbol): boolean;
    }

    /**
     * Holds all state needed for the finding references.
     * Unlike `Search`, there is only one `State`.
     */
    interface State extends Options {
        /** True if we're searching for constructor references. */
        readonly isForConstructor: boolean;

        readonly sourceFiles: SourceFile[];
        readonly checker: TypeChecker;
        readonly cancellationToken: CancellationToken;
        readonly searchMeaning: SemanticMeaning;

        /** Cache for `explicitlyinheritsFrom`. */
        readonly inheritsFromCache: Map<boolean>;

        /** Gets every place to look for references of an exported symbols. See `ImportsResult` in `importTracker.ts` for more documentation. */
        getImportSearches(exportSymbol: Symbol, exportInfo: ExportInfo): ImportsResult;

        /** @param allSearchSymbols set of additinal symbols for use by `includes`. */
        createSearch(location: Node, symbol: Symbol, comingFrom: ImportExport | undefined, searchOptions?: { text?: string, allSearchSymbols?: Symbol[] }): Search;

        /**
         * Callback to add references for a particular searched symbol.
         * This initializes a reference group, so only call this if you will add at least one reference.
         */
        referenceAdder(searchSymbol: Symbol, searchLocation: Node): (node: Node) => void;

        /** Add a reference with no associated definition. */
        addStringOrCommentReference(fileName: string, textSpan: TextSpan): void;

        /** Returns `true` the first time we search for a symbol in a file and `false` afterwards. */
        markSearchedSymbol(sourceFile: SourceFile, symbol: Symbol): boolean;

        /**
         * Type nodes can contain multiple references to the same type. For example:
         *      let x: Foo & (Foo & Bar) = ...
         * Because we are returning the implementation locations and not the identifier locations,
         * duplicate entries would be returned here as each of the type references is part of
         * the same implementation. For that reason, check before we add a new entry.
         */
        markSeenContainingTypeReference(containingTypeReference: Node): boolean;

        /**
         * It's possible that we will encounter either side of `export { foo as bar } from "x";` more than once.
         * For example:
         *     export { foo as bar } from "a";
         *     import { foo } from "a";
         *
         * Normally at `foo as bar` we directly add `foo` and do not locally search for it (since it doesn't declare a local).
         * But another reference to it may appear in the same source file.
         * See `tests/cases/fourslash/transitiveExportImports3.ts`.
         */
        markSeenReExportLHS(lhs: Identifier): boolean;
        markSeenReExportRHS(rhs: Identifier): boolean;
    }

    function createState(sourceFiles: SourceFile[], originalLocation: Node, checker: TypeChecker, cancellationToken: CancellationToken, searchMeaning: SemanticMeaning, options: Options, result: Push<ReferencedSymbol>): State {
        const symbolIdToReferences: ReferenceEntry[][] = [];
        const inheritsFromCache = createMap<boolean>();
        // Source file ID → symbol ID → Whether the symbol has been searched for in the source file.
        const sourceFileToSeenSymbols: Array<Array<true>> = [];
        const isForConstructor = originalLocation.kind === SyntaxKind.ConstructorKeyword;
        let importTracker: ImportTracker | undefined;

        return {
            ...options,
            sourceFiles, isForConstructor, checker, cancellationToken, searchMeaning, inheritsFromCache, getImportSearches, createSearch, referenceAdder, addStringOrCommentReference,
            markSearchedSymbol, markSeenContainingTypeReference: nodeSeenTracker(), markSeenReExportLHS: nodeSeenTracker(), markSeenReExportRHS: nodeSeenTracker(),
        };

        function getImportSearches(exportSymbol: Symbol, exportInfo: ExportInfo): ImportsResult {
            if (!importTracker) importTracker = createImportTracker(sourceFiles, checker);
            return importTracker(exportSymbol, exportInfo, options.isForRename);
        }

        function createSearch(location: Node, symbol: Symbol, comingFrom: ImportExport, searchOptions: { text?: string, allSearchSymbols?: Symbol[] } = {}): Search {
            // Note: if this is an external module symbol, the name doesn't include quotes.
            const { text = stripQuotes(getDeclaredName(checker, symbol, location)), allSearchSymbols = undefined } = searchOptions;
            const escapedText = escapeIdentifier(text);
            const parents = options.implementations && getParentSymbolsOfPropertyAccess(location, symbol, checker);
            return { location, symbol, comingFrom, text, escapedText, parents, includes };

            function includes(referenceSymbol: Symbol): boolean {
                return allSearchSymbols ? contains(allSearchSymbols, referenceSymbol) : referenceSymbol === symbol;
            }
        }

        function referenceAdder(referenceSymbol: Symbol, searchLocation: Node): (node: Node) => void {
            const symbolId = getSymbolId(referenceSymbol);
            let references = symbolIdToReferences[symbolId];
            if (!references) {
                references = symbolIdToReferences[symbolId] = [];
                result.push({ definition: getDefinition(referenceSymbol, searchLocation, checker), references });
            }
            return node => references.push(getReferenceEntryFromNode(node));
        }

        function addStringOrCommentReference(fileName: string, textSpan: TextSpan): void {
            result.push({
                definition: undefined,
                references: [{ fileName, textSpan, isWriteAccess: false, isDefinition: false }]
            });
        }

        function markSearchedSymbol(sourceFile: SourceFile, symbol: Symbol): boolean {
            const sourceId = getNodeId(sourceFile);
            const symbolId = getSymbolId(symbol);
            const seenSymbols = sourceFileToSeenSymbols[sourceId] || (sourceFileToSeenSymbols[sourceId] = []);
            return !seenSymbols[symbolId] && (seenSymbols[symbolId] = true);
        }
    }

    /** Core find-all-references algorithm for a normal symbol. */
    function getReferencedSymbolsForSymbol(symbol: Symbol, node: Node, sourceFiles: SourceFile[], checker: TypeChecker, cancellationToken: CancellationToken, options: Options): ReferencedSymbol[] {
        symbol = skipPastExportImportSpecifier(symbol, node, checker);

        // Compute the meaning from the location and the symbol it references
        const searchMeaning = getIntersectingMeaningFromDeclarations(getMeaningFromLocation(node), symbol.declarations);

        const result: ReferencedSymbol[] = [];
        const state = createState(sourceFiles, node, checker, cancellationToken, searchMeaning, options, result);
        const search = state.createSearch(node, symbol, /*comingFrom*/undefined, { allSearchSymbols: populateSearchSymbolSet(symbol, node, checker, options.implementations) });

        // Try to get the smallest valid scope that we can limit our search to;
        // otherwise we'll need to search globally (i.e. include each file).
        const scope = getSymbolScope(symbol);
        if (scope) {
            getReferencesInContainer(scope, search, state);
        }
        else {
            // Global search
            for (const sourceFile of state.sourceFiles) {
                state.cancellationToken.throwIfCancellationRequested();
                searchForName(sourceFile, search, state);
            }
        }

        return result;
    }

    /** Handle a few special cases relating to export/import specifiers. */
    function skipPastExportImportSpecifier(symbol: Symbol, node: Node, checker: TypeChecker): Symbol {
        const { parent } = node;
        if (isExportSpecifier(parent)) {
            if (parent.propertyName) {
                // `export { foo as bar } [from "someModule"]`
                if (parent.propertyName === node) {
                    // We're at `foo`. Find all refs on the original, not just on the import.
                    return checker.getShallowTargetOfExportSpecifier(symbol);
                }
                // We're at `bar`. Just find refs for that, not for `foo`.
                return symbol;
            }
            else {
                // If we're at `export { foo };`, don't start the search with the exported symbol; search with the local symbol.
                return getShallowTargetOfExportSpecifierIfNecessary(parent, symbol, checker);
            }
        }
        else if (isImportSpecifier(parent) && parent.propertyName === node) {
            // We're at `foo` in `import { foo as bar }`. Probably intended to find all refs on the original, not just on the import.
            return checker.getImmediateAliasedSymbol(symbol);
        }

        return symbol;
    }

    /** Search for all imports of a given exported symbol using `State.getImportSearches`. */
    function searchForImportsOfExport(exportLocation: Node, exportSymbol: Symbol, exportInfo: ExportInfo, state: State): void {
        const { importSearches, singleReferences, indirectUsers } = state.getImportSearches(exportSymbol, exportInfo);

        // For `import { foo as bar }` just add the reference to `foo`, and don't otherwise search in the file.
        if (singleReferences.length) {
            const addRef = state.referenceAdder(exportSymbol, exportLocation);
            for (const singleRef of singleReferences) {
                if (state.markSeenReExportLHS(singleRef)) {
                    addRef(singleRef);
                }
            }
        }

        // For each import, find all references to that import in its source file.
        for (const [importLocation, importSymbol] of importSearches) {
            state.cancellationToken.throwIfCancellationRequested();
            getReferencesInContainer(importLocation.getSourceFile(), state.createSearch(importLocation, importSymbol, ImportExport.Export), state);
        }

        if (indirectUsers.length) {
            const indirectSearch = (() => {
                switch (exportInfo.exportKind) {
                    case ExportKind.Named:
                        return state.createSearch(exportLocation, exportSymbol, ImportExport.Export);
                    case ExportKind.Default:
                        // Search for a property access to '.default'. This can't be renamed.
                        return state.isForRename ? undefined : state.createSearch(exportLocation, exportSymbol, ImportExport.Export, { text: "default" });
                    case ExportKind.ExportEquals:
                        return undefined;
                }
            })();
            if (indirectSearch) {
                for (const indirectUser of indirectUsers) {
                    state.cancellationToken.throwIfCancellationRequested();
                    searchForName(indirectUser, indirectSearch, state);
                }
            }
        }
    }

    /** Search for all occurences of an identifier in a source file (and filter out the ones that match). */
    function searchForName(sourceFile: SourceFile, search: Search, state: State): void {
        if (sourceFileHasName(sourceFile, search.escapedText)) {
            getReferencesInContainer(sourceFile, search, state);
        }
    }

    function sourceFileHasName(sourceFile: SourceFile, escapedName: string): boolean {
        return getNameTable(sourceFile).get(escapedName) !== undefined;
    }

    function getDefinition(symbol: Symbol, node: Node, checker: TypeChecker): ReferencedSymbolDefinitionInfo | undefined {
        const declarations = symbol.declarations;
        if (!declarations || declarations.length === 0) {
            return undefined;
        }

        const { displayParts, symbolKind } =
            SymbolDisplay.getSymbolDisplayPartsDocumentationAndSymbolKind(checker, symbol, node.getSourceFile(), getContainerNode(node), node);
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

    function getPropertySymbolOfDestructuringAssignment(location: Node, checker: TypeChecker): Symbol | undefined {
        return isArrayLiteralOrObjectLiteralDestructuringPattern(location.parent.parent) &&
            checker.getPropertySymbolOfDestructuringAssignment(<Identifier>location);
    }

    function isObjectBindingPatternElementWithoutPropertyName(symbol: Symbol): boolean {
        const bindingElement = <BindingElement>getDeclarationOfKind(symbol, SyntaxKind.BindingElement);
        return bindingElement &&
            bindingElement.parent.kind === SyntaxKind.ObjectBindingPattern &&
            !bindingElement.propertyName;
    }

    function getPropertySymbolOfObjectBindingPatternWithoutPropertyName(symbol: Symbol, checker: TypeChecker): Symbol | undefined {
        if (isObjectBindingPatternElementWithoutPropertyName(symbol)) {
            const bindingElement = <BindingElement>getDeclarationOfKind(symbol, SyntaxKind.BindingElement);
            const typeOfPattern = checker.getTypeAtLocation(bindingElement.parent);
            return typeOfPattern && checker.getPropertyOfType(typeOfPattern, (<Identifier>bindingElement.name).text);
        }
        return undefined;
    }

    /**
     * Determines the smallest scope in which a symbol may have named references.
     * Note that not every construct has been accounted for. This function can
     * probably be improved.
     *
     * @returns undefined if the scope cannot be determined, implying that
     * a reference to a symbol can occur anywhere.
     */
    function getSymbolScope(symbol: Symbol): Node | undefined {
        // If this is the symbol of a named function expression or named class expression,
        // then named references are limited to its own scope.
        const { declarations, flags, parent, valueDeclaration } = symbol;
        if (valueDeclaration && (valueDeclaration.kind === SyntaxKind.FunctionExpression || valueDeclaration.kind === SyntaxKind.ClassExpression)) {
            return valueDeclaration;
        }

        if (!declarations) {
            return undefined;
        }

        // If this is private property or method, the scope is the containing class
        if (flags & (SymbolFlags.Property | SymbolFlags.Method)) {
            const privateDeclaration = find(declarations, d => !!(getModifierFlags(d) & ModifierFlags.Private));
            if (privateDeclaration) {
                return getAncestor(privateDeclaration, SyntaxKind.ClassDeclaration);
            }
        }

        // If symbol is of object binding pattern element without property name we would want to
        // look for property too and that could be anywhere
        if (isObjectBindingPatternElementWithoutPropertyName(symbol)) {
            return undefined;
        }

        // The only symbols with parents *not* globally acessible are exports of external modules, and only then if they do not have `export as namespace`.
        if (parent && !((parent.flags & SymbolFlags.Module) && isExternalModuleSymbol(parent) && !parent.globalExports)) {
            return undefined;
        }

        // If this is a synthetic property, it's a property and must be searched for globally.
        if (flags & SymbolFlags.SyntheticProperty) {
            return undefined;
        }

        let scope: Node | undefined;
        for (const declaration of declarations) {
            const container = getContainerNode(declaration);
            if (scope && scope !== container) {
                // Different declarations have different containers, bail out
                return undefined;
            }

            if (!container || container.kind === SyntaxKind.SourceFile && !isExternalOrCommonJsModule(<SourceFile>container)) {
                // This is a global variable and not an external module, any declaration defined
                // within this scope is visible outside the file
                return undefined;
            }

            // The search scope is the container node
            scope = container;
        }

        // If symbol.parent, this means we are in an export of an external module. (Otherwise we would have returned `undefined` above.)
        // For an export of a module, we may be in a declaration file, and it may be accessed elsewhere. E.g.:
        //     declare module "a" { export type T = number; }
        //     declare module "b" { import { T } from "a"; export const x: T; }
        // So we must search the whole source file. (Because we will mark the source file as seen, we we won't return to it when searching for imports.)
        return parent ? scope.getSourceFile() : scope;
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
        for (const position of possiblePositions) {
            cancellationToken.throwIfCancellationRequested();

            const node = getTouchingWord(sourceFile, position);
            if (!node || node.getWidth() !== labelName.length) {
                continue;
            }

            // Only pick labels that are either the target label, or have a target that is the target label
            if (node === targetLabel ||
                (isJumpStatementTarget(node) && getTargetLabel(node, labelName) === targetLabel)) {
                references.push(getReferenceEntryFromNode(node));
            }
        }

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

    /**
     * Search within node "container" for references for a search value, where the search value is defined as a
     * tuple of(searchSymbol, searchText, searchLocation, and searchMeaning).
     * searchLocation: a node where the search value
     */
    function getReferencesInContainer(container: Node, search: Search, state: State): void {
        const sourceFile = container.getSourceFile();
        if (!state.markSearchedSymbol(sourceFile, search.symbol)) {
            return;
        }

        const start = state.findInComments ? container.getFullStart() : container.getStart();
        const possiblePositions = getPossibleSymbolReferencePositions(sourceFile, search.text, start, container.getEnd(), state.cancellationToken);
        for (const position of possiblePositions) {
            state.cancellationToken.throwIfCancellationRequested();
            getReferencesAtLocation(sourceFile, position, search, state);
        }
    }

    function getReferencesAtLocation(sourceFile: SourceFile, position: number, search: Search, state: State): void {
        const referenceLocation = getTouchingPropertyName(sourceFile, position);

        if (!isValidReferencePosition(referenceLocation, search.text)) {
            // This wasn't the start of a token.  Check to see if it might be a
            // match in a comment or string if that's what the caller is asking
            // for.
            if (!state.implementations && (state.findInStrings && isInString(sourceFile, position) || state.findInComments && isInNonReferenceComment(sourceFile, position))) {
                // In the case where we're looking inside comments/strings, we don't have
                // an actual definition.  So just use 'undefined' here.  Features like
                // 'Rename' won't care (as they ignore the definitions), and features like
                // 'FindReferences' will just filter out these results.
                state.addStringOrCommentReference(sourceFile.fileName, createTextSpan(position, search.text.length));
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

        if (isExportSpecifier(referenceLocation.parent)) {
            Debug.assert(referenceLocation.kind === SyntaxKind.Identifier);
            getReferencesAtExportSpecifier(referenceLocation as Identifier, referenceSymbol, referenceLocation.parent, search, state);
            return;
        }

        const relatedSymbol = getRelatedSymbol(search, referenceSymbol, referenceLocation, state);
        if (!relatedSymbol) {
            getReferenceForShorthandProperty(referenceSymbol, search, state);
            return;
        }

        if (state.isForConstructor) {
            findConstructorReferences(relatedSymbol, referenceLocation, sourceFile, search, state);
        }
        else {
            addReference(referenceLocation, relatedSymbol, search.location, state);
        }

        getImportOrExportReferences(referenceLocation, referenceSymbol, search, state);
    }

    function getImportOrExportReferences(referenceLocation: Node, referenceSymbol: Symbol, search: Search, state: State): void {
        const importOrExport = getImportOrExportSymbol(referenceLocation, referenceSymbol, state.checker, search.comingFrom === ImportExport.Export);
        if (!importOrExport) return;

        const { symbol } = importOrExport;

        if (importOrExport.kind === ImportExport.Import) {
            if (!(state.isForRename && !importOrExport.isNamedImport)) { //neater
                searchForImportedSymbol(symbol, state);
            }
        }
        else {
            // We don't check for `state.isForRename`, even for default exports, because importers that previously matched the export name should be updated to continue matching.
            searchForImportsOfExport(referenceLocation, symbol, importOrExport.info, state);
        }
    }

    // Go to the symbol we imported from and find references for it.
    //review uses
    function searchForImportedSymbol(symbol: Symbol, state: State): void {
        for (const declaration of symbol.declarations) { //test that we go to multiple declarations
            getReferencesInContainer(declaration.getSourceFile(), state.createSearch(declaration, symbol, ImportExport.Import), state);
        }
    }

    //name, move
    function getShallowTargetOfExportSpecifierIfNecessary(exportSpecifier: ExportSpecifier, referenceSymbol: Symbol, checker: TypeChecker): Symbol {
        const exportDeclaration = exportSpecifier.parent.parent;
        return exportDeclaration.moduleSpecifier ? referenceSymbol : checker.getShallowTargetOfExportSpecifier(referenceSymbol);
    }

    //Note: this recurses for uses of the export. It doesn't need to recurse to uses of the import because we skip past re-exports to the original import anyway.
    function getReferencesAtExportSpecifier(referenceLocation: Identifier, referenceSymbol: Symbol, exportSpecifier: ExportSpecifier, search: Search, state: State): void {
        if (exportSpecifier.propertyName) {
            // Given `export { foo as bar }`...
            if (exportSpecifier.propertyName === referenceLocation) {
                //We're at `foo`.
                //skipPastExportImportSpecifier should have skipped past this normally. Shouldn't need to add a reference.
                //Also, if this is a rename we won't want to look at rhs.
                //ReferenceSymbol will actually be the export symbol even though we're on the left-hand side.
                //const rhs = state.checker.getSymbolAtLocation(exportSpecifier.name);
                const aliased = state.checker.getExportSpecifierLocalTargetSymbol(exportSpecifier);
                f(aliased, referenceSymbol, exportSpecifier.name);
            }
            else {
                Debug.assert(exportSpecifier.name === referenceLocation);
                f(referenceSymbol, referenceSymbol);
            }
        }
        else {
            Debug.assert(exportSpecifier.name === referenceLocation);
            // Use the local `foo` rather than the exported one for comparison and for adding the reference.
            f(getShallowTargetOfExportSpecifierIfNecessary(exportSpecifier, referenceSymbol, state.checker), referenceSymbol);
        }

        const exportDeclaration = exportSpecifier.parent.parent;
        if (search.comingFrom !== ImportExport.Export && exportDeclaration.moduleSpecifier) {
            if (!exportSpecifier.propertyName) {
                const importedSymbol = state.checker.getExportSpecifierLocalTargetSymbol(exportSpecifier);
                searchForImportedSymbol(importedSymbol, state);
            }
        }

        //rename 'aliased' to 'localSymbol'
        function f(aliased: Symbol, exportSymbol: Symbol, renameExportRightHandSide?: Identifier) { //name
            if (search.includes(aliased)) { //TODO: if not, return
                if (!exportSpecifier.propertyName || (exportSpecifier.propertyName === referenceLocation ? state.markSeenReExportLHS : state.markSeenReExportRHS)(referenceLocation)) {
                    addReference(referenceLocation, aliased, search.location, state);
                }

                if (renameExportRightHandSide) {
                    //For `export { foo as bar }`, rename `foo`, but not `bar`.
                    if (state.isForRename) return;
                    //Add a reference for the rhs too
                    //ACTUALLy, we may have to search this file again. So use `singleReferences` to add this rather than adding immediately. `singleReferences` is filtered.
                    //ACTUALLY ACTUALLY, we just do a full search for one lousy symbol.
                    //ACTUALLY^3, just do it here and track duplicates.
                    if (state.markSeenReExportRHS(renameExportRightHandSide)) {
                        addReference(renameExportRightHandSide, exportSymbol, renameExportRightHandSide, state);
                    }
                }

                const exportInfo = getExportInfo(exportSymbol,
                    (referenceLocation as Identifier).originalKeywordKind === ts.SyntaxKind.DefaultKeyword ? ExportKind.Default : ExportKind.Named,
                    state.checker);
                Debug.assert(!!exportInfo);
                searchForImportsOfExport(referenceLocation, exportSymbol, exportInfo, state);
            }
        }
    }

    //neater
    function getReferenceForShorthandProperty(referenceSymbol: Symbol, search: Search, state: State): void {
        const referenceSymbolDeclaration = referenceSymbol.valueDeclaration;
        const shorthandValueSymbol = state.checker.getShorthandAssignmentValueSymbol(referenceSymbolDeclaration);
        /*
         * Because in short-hand property assignment, an identifier which stored as name of the short-hand property assignment
         * has two meanings: property name and property value. Therefore when we do findAllReference at the position where
         * an identifier is declared, the language service should return the position of the variable declaration as well as
         * the position in short-hand property assignment excluding property accessing. However, if we do findAllReference at the
         * position of property accessing, the referenceEntry of such position will be handled in the first case.
         */
        if (!(referenceSymbol.flags & SymbolFlags.Transient) && search.includes(shorthandValueSymbol)) {
            addReference(referenceSymbolDeclaration.name, shorthandValueSymbol, search.location, state);
        }
    }

    function addReference(referenceLocation: Node, relatedSymbol: Symbol, searchLocation: Node, state: State): void {
        const addRef = state.referenceAdder(relatedSymbol, searchLocation);
        if (state.implementations) {
            addImplementationReferences(referenceLocation, addRef, state);
        }
        else {
            addRef(referenceLocation);
        }
    }

    /** Adds references when a constructor is used with `new this()` in its own class and `super()` calls in subclasses.  */
    function findConstructorReferences(referenceSymbol: Symbol, referenceLocation: Node, sourceFile: SourceFile, search: Search, state: State): void {
        Debug.assert(referenceSymbol === search.symbol); //So don't pass it in!

        if (isNewExpressionTarget(referenceLocation)) {
            addReference(referenceLocation, referenceSymbol, search.location, state);
        }

        const classSymbol = skipAliases(search.symbol, state.checker);
        Debug.assert(isClassLike(classSymbol.valueDeclaration));
        const pusher = state.referenceAdder(search.symbol, search.location);

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


    function getPropertyAccessExpressionFromRightHandSide(node: Node): PropertyAccessExpression {
        return isRightSideOfPropertyAccess(node) && <PropertyAccessExpression>node.parent;
    }

    /**
     * `classSymbol` is the class where the constructor was defined.
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
        if (containingTypeReference && state.markSeenContainingTypeReference(containingTypeReference)) {
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

    // For certain symbol kinds, we need to include other symbols in the search set.
    // This is not needed when searching for re-exports.
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

    function getRelatedSymbol(search: Search, referenceSymbol: Symbol, referenceLocation: Node, state: State): Symbol | undefined {
        if (search.includes(referenceSymbol)) {
            return referenceSymbol;
        }

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
                if (search.parents && !some(search.parents, parent => explicitlyInheritsFrom(rootSymbol.parent, parent, state.inheritsFromCache, state.checker))) {
                    return undefined;
                }

                const result: Symbol[] = [];
                getPropertySymbolsFromBaseTypes(rootSymbol.parent, rootSymbol.getName(), result, /*previousIterationSymbolsCache*/ createMap<Symbol>(), state.checker);
                return find(result, search.includes);
            }

            return undefined;
        });
    }


    //ABOVE: CORE ALGORITHM
    //BELOW: HELPERS

    function getNameFromObjectLiteralElement(node: ObjectLiteralElement): string {
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

    function getTextSpan(node: Node): TextSpan {
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

    function forEachDescendantOfKind(node: Node, kind: SyntaxKind, action: (node: Node) => void): void {
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

    //NEW HELPERS BELOW

    function skipAliases(symbol: Symbol, checker: TypeChecker): Symbol {
        return symbol.flags & SymbolFlags.Alias ? checker.getAliasedSymbol(symbol) : symbol;
    }

    /**
     * If we are just looking for implementations and this is a property access expression, we need to get the
     * symbol of the local type of the symbol the property is being accessed on. This is because our search
     * symbol may have a different parent symbol if the local type's symbol does not declare the property
     * being accessed (i.e. it is declared in some parent class or interface)
     */
    function getParentSymbolsOfPropertyAccess(location: Node, symbol: Symbol, checker: TypeChecker): Symbol[] | undefined {
        const propertyAccessExpression = getPropertyAccessExpressionFromRightHandSide(location);
        if (!propertyAccessExpression) {
            return undefined;
        }

        const localParentType = checker.getTypeAtLocation(propertyAccessExpression.expression);
        if (!localParentType) {
            return undefined;
        }

        if (localParentType.symbol && localParentType.symbol.flags & (SymbolFlags.Class | SymbolFlags.Interface) && localParentType.symbol !== symbol.parent) {
            return [localParentType.symbol];
        }
        else if (localParentType.flags & TypeFlags.UnionOrIntersection) {
            return getSymbolsForClassAndInterfaceComponents(<UnionOrIntersectionType>localParentType);
        }
    }

    /** True if the symbol is for an external module, as opposed to a namespace. */
    export function isExternalModuleSymbol(moduleSymbol: Symbol): boolean {
        Debug.assert(!!(moduleSymbol.flags & SymbolFlags.Module));
        return moduleSymbol.name.charCodeAt(0) === CharacterCodes.doubleQuote;
    }

    /** Returns `true` the first time it encounters a node and `false` afterwards. */
    export function nodeSeenTracker<T extends Node>(): (node: T) => boolean {
        const seen: Array<true> = [];
        return node => {
            const id = getNodeId(node);
            return !seen[id] && (seen[id] = true);
        }
    }
}
