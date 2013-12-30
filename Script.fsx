// Learn more about F# at http://megasnippets.com/languages/fsharp.

#load "StringAgility.fs"
#load "WebAgility.fs"


open FsWebAgility.WebAgility

let html = """< div    data-key="toto"
                    data-model viewmodel='some \'view\' model'
                    width   = 650
                    data   =
                        "titi" last=some>
                <h1>Title <span class='b'>te<b>x</b>t </span></h1><hr/>
                <p style="font-style: \'italic\';">Content text</p><p>second content</p>
            </ div > <!--This is a comment bloc --><section class=sec/> """

// Failed test fixing in progress...
//let html = """
//              <script type='text/javascript'>/*<![CDATA[*/var thickboxL10n={"next":"Suiv.\u00a0>","prev":"<\u00a0Pr\u00e9c.","image":"Image","of":"sur","close":"Fermer","noiframes":"Cette fonctionnalit\u00e9 requiert des iframes. Les iframes sont d\u00e9sactiv\u00e9es sur votre navigateur, ou alors il ne les accepte pas.","loadingAnimation":"http:\/\/leprofdinfo.fr\/wp-includes\/js\/thickbox\/loadingAnimation.gif","closeImage":"http:\/\/leprofdinfo.fr\/wp-includes\/js\/thickbox\/tb-close.png"};/*]]>*/</script>"""


HtmlNodes html |> Seq.iter (fun p -> printfn "%A" p)
HtmlTags "b" html |> Seq.iter (fun p -> printfn "%A" p)


