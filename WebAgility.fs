//namespace WebAgility
module WebAgility

open StringAgility

// Read chars from input until next whitespace char (' ', '\n', '\t'...)
let rec ReadToWhitespace input =
    match input with
    | PrefixFirst (first, after)    -> match first with
                                        | WhiteSpace    -> Some(System.String.Empty, input)
                                        | _             ->
                                            let read = ReadToWhitespace after
                                            if read.IsSome then
                                                let (text, afterText) = read.Value
                                                Some(first + text, afterText)
                                            else
                                                None
    | _                             -> None

// Read chars located into a literal string until next string delimiter : sep
let rec ReadIntoString sep input =
    let escaptedSep = "\\" + sep
    match input with
    | IsEmpty                   -> Some(System.String.Empty, System.String.Empty)
    | Prefix sep after          -> Some(System.String.Empty, after)
    | Prefix escaptedSep after  ->
        let next = ReadIntoString sep after
        if next.IsSome then
            let (str,safter) = next.Value
            Some(escaptedSep + str, safter)
        else
            None
    | _                         ->
        let next = ReadIntoString sep (TrimFirst input)
        if next.IsSome then
            let (str, safter) = next.Value
            Some((FirstChar input) + str, safter)
        else
            None

// Read an attribute value at beginning of the input. input must starts with "="
let rec ReadAttributeValue input =
    match input with
    | IsEmpty                       -> None
    // String detected starting with a single quote
    | Prefix "'" after              -> ReadIntoString "'" after
    // String detected starting with a double quote
    | Prefix @"""" after            -> ReadIntoString @"""" after
    // String guess without any quotes -> read to first whitespace char
    | _                             -> ReadToWhitespace input

let (|AttributeChar|_|) input =
    match input with
    | Prefix "_" after  -> Some("_", after)
    | Prefix "-" after  -> Some("-", after)
    | _ when input <> null && input.Length >= 1 && System.Char.IsLetter(input.[0])
                        -> Some(input.Substring(0, 1), input.Substring(1))
    | _                 -> None

let (|AttributeDelimiter|_|) html =
    match html with
    | PrefixFirstSpace after
    | Prefix ">" after
    | Prefix "=" after          -> Some(TrimStartSpaces html)
    | _                         -> None

// Read an attribute name
let rec ReadAttributeName html =
    match html with
    | AttributeDelimiter after          -> Some(System.String.Empty, after)
    | AttributeChar (attrChar, after)   ->
        let str = ReadAttributeName after
        if str.IsSome then
            let (attr, afterAttr) = str.Value
            Some(attrChar + attr, afterAttr)
        else
            None
    | _                                 -> None

// Read one attribute with its value if exists
let ReadAttribute html =
    let attributeName = ReadAttributeName (TrimStartSpaces html)
    if attributeName.IsSome then
        let (name, afterName) = attributeName.Value
        // Attribute name read, now start reading attribute value if exists
        let hasValue = match FirstChar afterName with | "=" -> true | _ -> false
        if hasValue then
            let attributeValue = ReadAttributeValue (TrimStartSpaces (TrimFirst afterName))
            if attributeValue.IsSome then
                let (value, afterValue) = attributeValue.Value
                Some(name, Some(value), afterValue)
            else
                None
        else
            Some(name, None, afterName)
    else
        None

// Provides a simple structure for HTML node attribute
type Attribute = {Name:string; Value:string option}

// Try match an HTML node attribute from html
let (|Attribute|_|) html =
    let attr = ReadAttribute html
    if attr.IsSome then
        let (name, value, after) = attr.Value
        Some({Name=name; Value=value}, after)
    else
        None

// Read HTML node attributes from html
let rec ReadAttributes html =
    match html with
    | Prefix ">" after          -> []
    | Attribute (attr, after)   -> attr::ReadAttributes after
    | _                         -> []

// Read a HTML node from html
let rec ReadNode html =
    match html with
    // All nodes start with a '<' char
    | Prefix "<" after              -> ReadNode after
    // Any whitespace char means the end of node name (stop recursivity)
    | PrefixFirstSpace after        -> Some(System.String.Empty, ReadAttributes after)
    // Any first+after pattern build a node name
    | PrefixFirst (first, after)    ->
        let str = ReadNode after
        if str.IsSome then
            let (name, attrs) = str.Value
            Some(first + name, attrs)
        else
            None
    | _                         -> None



