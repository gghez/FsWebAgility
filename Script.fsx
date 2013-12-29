// Learn more about F# at http://megasnippets.com/languages/fsharp.

#load "StringAgility.fs"
#load "WebAgility.fs"


open FsWebAgility.WebAgility

let html = "< div    data-key=\"toto\"
                    data-model viewmodel='some \"view\" model'
                    width   = 650
                    data   =
                        \"titi\" last=some>
                <h1>Title <span class='b'>te<b>x</b>t </span></h1><hr/>
                <p style=\"font-style: italic;\">Content text</p><p>second content</p>
            </ div > <!--This is a comment bloc --><section class=\"sec\" /> "


let elements = ReadElements html

type Tag(name:string, attrs:Attribute list, childs: Tag list) =
    new(name:string) = Tag(name, [], [])

let rec FindElements eltname (elements:Element list) =
    match elements with
    | elt::after    -> 
        match elt with
        | NodeDefinition(name, attrs, autoclosed) when name = eltname   -> NodeDefinition(name, attrs, autoclosed)::FindElements eltname after
        | _                                                             -> FindElements eltname after
    | _             -> []

let paragraphs = FindElements "p" elements

paragraphs |> Seq.iter (fun p -> printfn "%A" p)



//ReadHtml "http://leprofdinfo.fr"
//    |> Async.RunSynchronously