// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "StringAgility.fs"
#load "WebAgility.fs"


open WebAgility
open StringAgility

let attrsHtml = " data-key=\"toto\" data-model viewmodel='some \"view\" model' \r\n width\t= 650  data   =\n \"titi\" last"

printfn "attributes = %A" attrsHtml
let attrs = ReadAttributes attrsHtml
attrs |> Seq.iter (fun attr -> printfn "(%s, %A)" attr.Name attr.Value)

let html = "<div" + attrsHtml + ">"
let node = ReadNode html
printfn "%A" node
