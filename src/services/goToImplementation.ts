/* @internal */
namespace ts.GoToImplementation {
    export function getImplementationAtPosition(typeChecker: TypeChecker, cancellationToken: CancellationToken, sourceFiles: SourceFile[], node: Node): ImplementationLocation[] {
        const referenceEntries = getImplementationReferenceEntries(typeChecker, cancellationToken, sourceFiles, node);
        return undefinedIfEmpty(map(referenceEntries, ({ textSpan, fileName }) => ({ textSpan, fileName })));
    }

    function getImplementationReferenceEntries(typeChecker: TypeChecker, cancellationToken: CancellationToken, sourceFiles: SourceFile[], node: Node): ReferenceEntry[] {
        // If invoked directly on a shorthand property assignment, then return
        // the declaration of the symbol being assigned (not the symbol being assigned to).
        if (node.parent.kind === SyntaxKind.ShorthandPropertyAssignment) {
            const result: ReferenceEntry[] = [];
            FindAllReferences.getReferenceEntriesForShorthandPropertyAssignment(node, typeChecker, node =>
                result.push(FindAllReferences.getReferenceEntryFromNode(node)));
            return result;
        }
        else if (node.kind === SyntaxKind.SuperKeyword || isSuperProperty(node.parent)) {
            // References to and accesses on the super keyword only have one possible implementation, so no
            // need to "Find all References"
            const symbol = typeChecker.getSymbolAtLocation(node);
            return symbol.valueDeclaration && [FindAllReferences.getReferenceEntryFromNode(symbol.valueDeclaration)];
        }
        else {
            // Perform "Find all References" and retrieve only those that are implementations
            const referencedSymbols = FindAllReferences.getReferencedSymbolsForNode(typeChecker, cancellationToken,
                node, sourceFiles, /*findInStrings*/false, /*findInComments*/false, /*isForRename*/false, /*implementations*/true);
            //findallrefs will do this step in getreferencedentries!!!
            return FindAllReferences.convertReferences(referencedSymbols);
        }
    }

    function undefinedIfEmpty<T>(arr: T[] | undefined): T[] | undefined {
        return arr && arr.length ? arr : undefined;
    }
}
