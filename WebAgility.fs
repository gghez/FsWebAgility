namespace FsWebAgility

open FsWebAgility.StringAgility

module WebAgility =

    // Read chars from input until next value delimiter (any white-space or '>')
    let rec ReadIntoLiteral input =
        match input with
        | PrefixFirstSpace after
        | Prefix "/" after
        | Prefix ">" after              -> (System.String.Empty, input)
        | PrefixFirst (first, after)    ->
            let (text, afterText) = ReadIntoLiteral after
            (first + text, afterText)
        | _                             -> (System.String.Empty, input)

    // Read chars located into a literal string until next string delimiter
    let ReadString input =
        let rec ReadIntoDelimitedString sep input =
            let escaptedSep = "\\" + sep
            match input with
            // Match input end (so string end)
            | IsEmpty                   -> (System.String.Empty, System.String.Empty)
            // Match explicit end
            | Prefix sep after          -> (System.String.Empty, after)
            // Match escaped sep to avoid closing string before real end
            | Prefix escaptedSep after  ->
                let (str,safter) = ReadIntoDelimitedString sep after
                (escaptedSep + str, safter)
            | _                         ->
                let (str, safter) = ReadIntoDelimitedString sep (TrimFirst input)
                ((FirstChar input) + str, safter)
        ReadIntoDelimitedString (FirstChar input) (TrimFirst input)

    // Read an attribute value (string or literal) from input
    let ReadAttributeValue input =
        match input with
        // String detected starting with a single quote
        | Prefix "'" after
        // String detected starting with a double quote
        | Prefix @"""" after            -> ReadString input
        // String guess without any quotes -> read to first whitespace char
        | _                             -> ReadIntoLiteral input

    // Try match a node name or attribute name char from input
    let (|NameChar|_|) input =
        match input with
        | PrefixFirstLetterOrDigit (l, after)   -> Some(l, after)
        | Prefix "_" after                      -> Some("_", after)
        | Prefix "-" after                      -> Some("-", after)
        | _                                     -> None

    // Try match first char of html with a known attribute char delimiter
    let (|AttributeNameDelimiter|_|) html =
        match html with
        | PrefixFirstSpace after    -> Some(after)
        | Prefix ">" after
        | Prefix "/" after
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
        let attributeName = ReadAttributeName html
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
        | PrefixAllSpaces after    -> ReadAttributes after
        | Prefix "/" after
        | Prefix ">" after          -> ([], html)
        | Attribute (attr, after)   ->
            let (readAttr, readAfter) = ReadAttributes after
            (attr::readAttr, readAfter)
        | _                         -> ([], html)

    // Try match first char of html with a known node-name char delimiter
    let (|NodeNameDelimiter|_|) html =
        match html with
        | PrefixFirstSpace after    -> Some(after)
        | Prefix "/" after
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

    let rec ReadAutoClosedNode input =
        match input with
        | Prefix ">" after      -> (System.String.Empty, after)
        | PrefixAllSpaces after -> ReadAutoClosedNode after
        | _                     ->
            let (name, afterName) = ReadNodeName input
            let (acName, acNameAfter) = ReadAutoClosedNode afterName
            (name + acName, acNameAfter)

    let rec ReadCommentNode input =
        match input with
        | Prefix "-->" after
        | Prefix ">" after              -> (System.String.Empty, after)
        | PrefixFirst (first, after)    ->
            let (comment, commentAfter) = ReadCommentNode after
            (first + comment, commentAfter)
        | _                             -> (System.String.Empty, System.String.Empty)

    let rec ReadNodeDefinition input =
        match input with
        | Prefix ">" after              -> (System.String.Empty, [], false, after)
        | Prefix "/" after              ->
            let (name, attrs, _, afterDef) = ReadNodeDefinition after
            (name, attrs, true, afterDef)
        | PrefixAllSpaces after         -> ReadNodeDefinition after
        | _                             ->
            let (name, afterName) = ReadNodeName input
            let (attrs, afterAttrs) = ReadAttributes afterName
            let (_, _, autoclosed, afterDef) = ReadNodeDefinition afterAttrs
            (name, attrs, autoclosed, afterDef)

    // Provides a simple structure for HTML element
    type HtmlNode =
        | NodeDefinition of string * Attribute list * bool // name, attributes, autoclosed
        | NodeClosing of string // name
        | Literal of string // content
        | Comment of string // comment
        | Unknown of string // any other content (should not happen)

    // Try match a HTML tag with html
    let (|Tag|_|) html =
        match html with
        | Prefix "<" after  ->
            match TrimStartSpaces after with
            // match with auto-closed node
            | Prefix "/" afterSlash         ->
                let (name, nameAfter) = ReadAutoClosedNode afterSlash
                Some(NodeClosing(name), nameAfter)
            // match with comment node
            | Prefix "!--" afterCommentMark
            | Prefix "!" afterCommentMark   ->
                let (comment, commentAfter) = ReadCommentNode afterCommentMark
                Some(Comment(comment), commentAfter)
            // match with any other node type
            | _                             ->
                let (name, attrs, autoClosed, afterDefinition) = ReadNodeDefinition after
                Some(NodeDefinition(name, attrs, autoClosed), afterDefinition)
        | _                 -> None

    let ReadScriptComment html =
        let (commentSep, commentSepAfter) =
            match html with
            | Prefix "//" after -> ("\n", after)
            | Prefix "/*" after -> ("*/", after)
            | _                 -> (System.String.Empty, html)

        let rec ReadScriptDelimitedComment sep html =
            match html with
            | Prefix sep after              -> (sep, after)
            | PrefixFirst (first, after)    ->
                let (comment, commentAfter) = ReadScriptDelimitedComment sep after
                (first + comment, commentAfter)
            | _                             -> (System.String.Empty, System.String.Empty)

        ReadScriptDelimitedComment commentSep commentSepAfter

    let (|ScriptString|_|) script =
        match script with
        | Prefix "'" after
        | Prefix @"""" after    -> Some(ReadString script)
        | _                     -> None

    let (|ScriptComment|_|) script =
        match script with
        | Prefix "/*" after
        | Prefix "//" after -> Some(ReadScriptComment script)
        | _                 -> None

    // Reads a script tag content from html which one may contain // or /* */ comments
    let rec ReadScript html =
        match html with
        | Prefix "<" after                  -> (System.String.Empty, html)
        | ScriptString (str, after)         ->
            let (script, scriptAfter) = ReadScript after
            (str + script, scriptAfter)
        | ScriptComment (comment, after)    ->
            let (script, scriptAfter) = ReadScript after
            (comment + script, scriptAfter)
        | PrefixFirst (first, after)        ->
            let (script, scriptAfter) = ReadScript after
            (first + script, scriptAfter)
        | _                                 -> (System.String.Empty, System.String.Empty)

    // Try match a html fragment with an between-nodes literal
    // eg: <title>Page title</title> contains the "Page title" literal
    let (|Literal|_|) html =
        match html with
        | Prefix "<" after                  -> None
        | Contains "<" (before, after)      -> Some(HtmlNode.Literal(before), "<" + after)
        | _                                 -> None

    // Iterate through html and yield a node whenever discovered
    let rec HtmlNodes html =
        match html with
        | Tag (node, after)             -> seq {
                                                yield node
                                                match node with
                                                // Try match <script> special tag which may contain html tag included in comments or strings
                                                |   NodeDefinition("script", _, _)  ->
                                                    let (script, scriptAfter) = ReadScript after
                                                    match script with
                                                    | ""    -> () |> ignore
                                                    | _     -> yield HtmlNode.Literal(script)
                                                    yield! HtmlNodes scriptAfter
                                                | _                                 ->
                                                    yield! HtmlNodes after
                                            }
        | Literal (node, after)         -> seq {
                                                yield node
                                                yield! HtmlNodes after
                                            }
        | PrefixFirst (first, after)    -> seq { yield! HtmlNodes after }
        | _                             -> Seq.empty

    // Provides a type for a HTML tag (eg: <p style="..."> ...</p>)
    type HtmlTag(name:string, attrs:Attribute list, parent:HtmlTag option) =
        new(name) = HtmlTag(name, [], None)
        new(name, attrs) = HtmlTag(name, attrs, None)

        member this.Name  = name
        member this.Attributes = attrs
        member this.Parent = parent
        member this.Childs = ()
        //{ Name:string; Attributes:Attribute list; Parent:HtmlTag option}

    // Find all tags inside html matching tagName as name
    let HtmlTags tagName html =
        let rec FindTags parent tagName (elements:HtmlNode list) =
            match elements with
            | NodeDefinition(name, attrs, autoclosed)::after    ->
                let tag = HtmlTag(name, attrs, parent)
                if name = tagName then
                    match autoclosed with
                    // Use current parent as parent for next found tags
                    | true  -> tag::FindTags parent tagName after
                    // Use current tag as parent for next found tags
                    | false -> tag::FindTags (Some tag) tagName after
                else
                    FindTags (Some tag) tagName after
            | element::after                                    -> FindTags parent tagName after
            | _                                                 -> []
        FindTags None tagName (Seq.toList (HtmlNodes html))

    // Load a HTML content from an URL
    let LoadHtml url = async{
        use wc = new System.Net.WebClient()
        return! wc.AsyncDownloadString(new System.Uri(url))
    }

    // Iterate nodes inside HTML got thru url
    let UrlNodes url = async{
        let! html = LoadHtml url
        return HtmlNodes html
    }

    // Find tags inside HTML got thru url
    let UrlTags tagName url = async{
        let! html = LoadHtml url
        return HtmlTags tagName html
    }

