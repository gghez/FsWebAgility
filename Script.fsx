// Learn more about F# at http://megasnippets.com/languages/fsharp.

#load "StringAgility.fs"
#load "WebAgility.fs"


open FsWebAgility.WebAgility

//let html = """< div    data-key="toto"
//                    data-model viewmodel='some \'view\' model'
//                    width   = 650
//                    data   =
//                        "titi" last=some>
//                <h1>Title <span class='b'>te<b>x</b>t </span></h1><hr/>
//                <p style="font-style: \'italic\';">Content with <a href="http://megasnippets.com"></ a></p><p>second content</p>
//            </ div > <!--This is a comment bloc --><section class=sec/> """
//
//
//HtmlNodes html |> Seq.iter (fun p -> printfn "%A" p)
//HtmlTags "a" html |> Seq.iter (fun p -> printfn "%A" p)
//
//UrlTags "a" "http://leprofdinfo.fr"
//    |> Async.RunSynchronously
//    |> Seq.map (fun tag -> tag.Attributes |> Seq.pick (fun attr -> if attr.Name = "href" then Some(attr.Value) else None))
//    |> Seq.iter (fun attr -> printfn "%A" attr)


open Microsoft.FSharp.Core.CompilerServices

#load "ProvidedTypes\Code\ProvidedTypes.fs"
open ProviderImplementation.ProvidedTypes


type WebAgilityContainer(url:string) =
    member this.Url = url
    member this.Nodes = UrlNodes url |> Async.RunSynchronously

[<TypeProvider>]
type FsWebAgilityTypeProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()
    
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "FsWebAgility"

    let mainType = ProvidedTypeDefinition(asm, ns, "WebTree", Some(typeof<obj>))

    let urlParameter = ProvidedStaticParameter("url", typeof<string>)

    do mainType.DefineStaticParameters([urlParameter], fun typeName parameters ->

        let providedType = ProvidedTypeDefinition(asm, ns, typeName, None)
        let url = string parameters.[0]

        // Add a parameterless constructor that loads the file that was used to define the schema.
        let ``constructor`` = ProvidedConstructor([],
                                    InvokeCode = fun exprs -> <@@ WebAgilityContainer(url) @@>)
        providedType.AddMember ``constructor``

        let nodesProperty = ProvidedProperty("Nodes", typedefof<seq<HtmlNode>>, 
                                    GetterCode = fun exprs -> match exprs with
                                                    | container::[] -> <@@ (%%container:WebAgilityContainer).Nodes @@>
                                                    | _             -> <@@ Seq.empty @@> )
        providedType.AddMember nodesProperty

        providedType)
    // Add the type to the namespace.
    do this.AddNamespace(ns, [mainType])

[<assembly:TypeProviderAssembly>] 
do()

//let tree = new WebTree<"http://leprofdinfo.fr">()
//tree.Nodes