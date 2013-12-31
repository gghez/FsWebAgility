namespace FsWebAgility

module StringAgility =

    let StartsWith (str:string) (input:string) = input <> null && str <> null && input.Length >= str.Length && input.StartsWith(str, System.StringComparison.Ordinal)
    let EndsWith (str:string) (input:string) = input <> null && str <> null && input.Length >= str.Length && input.EndsWith(str, System.StringComparison.Ordinal)

    // Remove first n chars from a string
    let TrimNStart n (input:string) = input.Substring(n)
    // Remove last n chars from a string
    let TrimNEnd n (input:string) = input.Substring(0, input.Length - n)
    // Remove first char from a string
    let TrimFirst = TrimNStart 1
    // Remove last char from a string
    let TrimLast = TrimNEnd 1

    // Remove all leading and trailing spaces from input
    let TrimSpaces (input:string) = if input <> null then input.Trim() else null
    // Remove all leading spaces from input
    let TrimStartSpaces (input:string) = if input <> null then input.TrimStart() else null
    // Remove all trailing spaces from input
    let TrimEndSpaces (input:string) = if input <> null then input.TrimEnd() else null

    // Provides the first char of input
    let FirstChar (input:string) = input.Substring(0, 1)

    // Provides the last char of input
    let LastChar (input:string) = input.Substring(input.Length - 1)

    // Try match input with a white-space char
    let (|WhiteSpace|_|) input =
        match input with
        | " " | "\n" | "\t" | "\r"  -> Some()
        | _                         -> None

    // Provides a boolean value indicating whether input is null/empty or not
    let (|IsEmpty|_|) (input:string) = if System.String.IsNullOrEmpty(input) then Some() else None

    // Match input with prefix+after and provides after
    let (|Prefix|_|) prefix input =
        if StartsWith prefix input then
            Some(input.Substring(prefix.Length))
        else
            None

    // Match input with before+postfix and provides before
    let (|Postfix|_|) postfix input =
        if EndsWith postfix input then
            Some(input.Substring(0, input.Length - postfix.Length))
        else
            None

    // Match input with (any char)+after and provides after
    let (|PrefixFirst|_|) (input:string) =
        if input <> null && input.Length >= 1 then
            Some(FirstChar input, TrimFirst input)
        else
            None

    let (|PrefixFirstSpace|_|) input =
        match input with
        | PrefixFirst (first, after)    -> match first with | WhiteSpace -> Some(after) | _ -> None
        | _                             -> None

    // 
    let (|PrefixAllSpaces|_|) input =
        match input with
        | PrefixFirstSpace after    -> Some(TrimStartSpaces after)
        | _                         -> None

    // Match input with before+(any char) and provides before  
    let (|PostfixLast|_|) (input:string) =
        if input <> null && input.Length >= 1 then
            Some(TrimLast input, LastChar input)
        else
            None

    // Match input with before+needle+after and provides (before, after) tuple
    let (|Contains|_|) (needle:string) (input:string) =
        if input <> null && needle <> null then
            let needleIndex = input.IndexOf(needle)
            if needleIndex >= 0 then
                Some(input.Substring(0, needleIndex), input.Substring(needleIndex + needle.Length))
            else
                None
        else
            None

    // Try match input with prefix+str+suffix and provides str
    let (|Match|_|) prefix postfix (input:string) =
        match input with
        | Prefix prefix afterPrefix -> match afterPrefix with
                                        | Postfix postfix beforePostfix -> Some(beforePostfix)
                                        | _                             -> None
        | _                         -> None

    // Try match input with needle+str+needle and provides str
    let (|SurroundedWith|_|) needle input =
        match input with
        | Match needle needle surrounded    -> Some(surrounded)
        | _                                 -> None


    let (|PrefixFirstLetter|_|) (input:string) =
        match input with
        | PrefixFirst (first, after)    -> if System.Char.IsLetter(first.[0]) then Some(first, after) else None
        | _                             -> None

    let (|PrefixFirstDigit|_|) (input:string) =
        match input with
        | PrefixFirst (first, after)    -> if System.Char.IsDigit(first.[0]) then Some(first, after) else None
        | _                             -> None

    let (|PrefixFirstLetterOrDigit|_|) (input:string) =
        match input with
        | PrefixFirst (first, after)    -> if System.Char.IsLetterOrDigit(first.[0]) then Some(first, after) else None
        | _                             -> None

