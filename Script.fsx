// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "StringAgility.fs"
#load "WebAgility.fs"


open WebAgility
open StringAgility

let attrs1 = "data-key=\"toto\" \r\n width\t= 650  data   =\n \"titi\" "


ReadAttributes attrs1
    |> Seq.iter (fun attr -> printfn "%A" (attr.Name, attr.Value))

