//namespace WebAgility
module WebAgility

open StringAgility

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
    | Prefix "=" after              -> ReadAttributeValue (TrimStartSpaces after)
    // String detected starting with a single quote
    | Prefix "'" after              -> ReadIntoString "'" after
    // String detected starting with a double quote
    | Prefix @"""" after            -> ReadIntoString @"""" after
    // String guess without any quotes -> read to first whitespace char
    | _                             -> ReadToWhitespace input

// Read an attribute name
let rec ReadAttributeName html =
    match html with
    | PrefixFirst (first, after)    -> match first with
                                        | WhiteSpace    -> ReadAttributeName (TrimStartSpaces after)
                                        | ">"           -> Some(System.String.Empty, false, html)
                                        | "="           -> Some(System.String.Empty, true, html)
                                        | _             ->
                                            let read = ReadAttributeName after
                                            if read.IsSome then
                                                let (word, hasValue, wafter) = read.Value
                                                Some(first + word, hasValue, wafter)
                                            else
                                                None
    | _                             -> None

// Read one attribute with its value if exists
let ReadAttribute html =
    let attributeName = ReadAttributeName (TrimStartSpaces html)
    if attributeName.IsSome then
        let (name, hasValue, afterName) = attributeName.Value
        // Attribute name read, now start reading attribute value if exists
        if hasValue then
            let attributeValue = ReadAttributeValue (TrimStartSpaces afterName)
            if attributeValue.IsSome then
                let (value, afterValue) = attributeValue.Value
                Some(name, value, afterValue)
            else
                None
        else
            None
    else
        None

type Attribute = {Name:string; Value:string}

let (|Attribute|_|) html =
    let attr = ReadAttribute html
    if attr.IsSome then
        let (name, value, after) = attr.Value
        Some({Name=name; Value=value}, after)
    else
        None

let rec ReadAttributes html =
    match html with
    | Attribute (attr, after)   -> attr::ReadAttributes after
    | _                         -> []

