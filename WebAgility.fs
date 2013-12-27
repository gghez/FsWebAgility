//namespace WebAgility
module WebAgility

open StringAgility

// Read chars from input until next value delimiter (any white-space or '>')
let rec ReadIntoLiteral input =
    match input with
    | PrefixFirstSpace after
    | Prefix ">" after              -> (System.String.Empty, input)
    | PrefixFirst (first, after)    ->
        let (text, afterText) = ReadIntoLiteral after
        (first + text, afterText)
    | _                             -> (System.String.Empty, input)

// Read chars located into a literal string until next string delimiter : sep
let rec ReadIntoString sep input =
    let escaptedSep = "\\" + sep
    match input with
    // Match input end (so string end)
    | IsEmpty                   -> (System.String.Empty, System.String.Empty)
    // Match explicit end
    | Prefix sep after          -> (System.String.Empty, after)
    // Match escaped sep to avoid closing string before real end
    | Prefix escaptedSep after  ->
        let (str,safter) = ReadIntoString sep after
        (escaptedSep + str, safter)
    | _                         ->
        let (str, safter) = ReadIntoString sep (TrimFirst input)
        ((FirstChar input) + str, safter)

// Read an attribute value (string or literal) from input
let ReadAttributeValue input =
    match input with
    // String detected starting with a single quote
    | Prefix "'" after              -> ReadIntoString "'" after
    // String detected starting with a double quote
    | Prefix @"""" after            -> ReadIntoString @"""" after
    // String guess without any quotes -> read to first whitespace char
    | _                             -> ReadIntoLiteral input

// Try match a node name or attribute name char from input
let (|NameChar|_|) input =
    match input with
    | LetterOrDigit (l, after)  -> Some(l, after)
    | Prefix "_" after          -> Some("_", after)
    | Prefix "-" after          -> Some("-", after)
    | _                         -> None

// Try match first char of html with a known attribute char delimiter
let (|AttributeNameDelimiter|_|) html =
    match html with
    | PrefixFirstSpace after    -> Some(after)
    | Prefix ">" after          
    | Prefix "=" after          -> Some(html)
    | _                         -> None

// Read an attribute name
let rec ReadAttributeName html =
    match html with
    | AttributeNameDelimiter after  -> Some(System.String.Empty, after)
    | NameChar (attrChar, after)    ->
        let read = ReadAttributeName after
        if read.IsSome then
            let (attr, afterAttr) = read.Value
            Some(attrChar + attr, afterAttr)
        else // If not able to read after this point the name cannot be determined
            None
    | _                             -> None

// Provides a simple structure for HTML node attribute
type Attribute = {Name:string; Value:string option}

// Try match an HTML node attribute with its value (if exists) from html
let (|Attribute|_|) html =
    let attributeName = ReadAttributeName (TrimStartSpaces html)
    if attributeName.IsSome then
        let (name, afterName) = attributeName.Value
        // Attribute name read, now start reading attribute value if exists
        let valueHtml =
            match TrimStartSpaces afterName with
            | Prefix "=" afterEqual -> Some(TrimStartSpaces afterEqual)
            | _                     -> None

        if valueHtml.IsSome then
            let (value, afterValue) = ReadAttributeValue valueHtml.Value
            Some({Name=name; Value=Some(value)}, afterValue)
        else
            Some({Name=name; Value=None}, afterName)
    else
        None

// Read HTML node attributes from html
let rec ReadAttributes html =
    match html with
    | Prefix ">" after          -> ([], after)
    | Attribute (attr, after)   ->
        let (readAttr, readAfter) = ReadAttributes after
        (attr::readAttr, readAfter)
    | _                         -> ([], html)

// Try match first char of html with a known node-name char delimiter
let (|NodeNameDelimiter|_|) html =
    match html with
    | PrefixFirstSpace after    -> Some(after)
    | Prefix ">" after          -> Some(html)
    | _                         -> None

// Read a HTML node name from html
let rec ReadNodeName html =
    match html with
    | NodeNameDelimiter after   -> (System.String.Empty, after)
    | NameChar (letter, after)  ->
        let (name, nameAfter) = ReadNodeName after
        (letter + name, nameAfter)
    | _                         -> (System.String.Empty, html)

// Provides a simple structure for HTML node
//type Node = {Name:string; Attributes:Attribute list; }
type Node =
    | NodeDefinition of string * Attribute list
    | NodeClosing of string
    | NodeText of string
    | NodeComment of string

// Try match a HTML node definition with html (eg: <div style="...">)

let (|Node|_|) html =
    match html with
    | Prefix "<" after  ->
        match TrimStartSpaces after with
        | Prefix "/" afterSlash         ->
            let (name, nameAfter) = ReadNodeName afterSlash
            Some(NodeClosing(name), nameAfter)
        | _                             ->
            let (name, nameAfter) = ReadNodeName after
            let (attrs, attrsAfter) = ReadAttributes (TrimStartSpaces nameAfter)
            Some(NodeDefinition(name, attrs), attrsAfter)
    | _                 -> None

let (|TextNode|_|) html =
   match html with
    | Prefix "<" after                  -> None
    | PrefixUntil "<" (before, after)   -> Some(NodeText(before), "<" + after)
    | _                                 -> None

let rec ReadNode html =
    match html with
    | Prefix ">" after              -> ReadNode after
    | Node (node, after)
    | TextNode (node, after)        -> node::ReadNode after
    | PrefixFirst (first, after)    -> ReadNode after
    | _                             -> []
