// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "StringAgility.fs"
#load "WebAgility.fs"


open WebAgility
open StringAgility

let html = "<div    data-key=\"toto\"
                    data-model viewmodel='some \"view\" model'
                    width   = 650
                    data   =
                        \"titi\" last=some>
                <h1>Title <span class='b'>te<b>x</b>t </span></h1><hr/>
                <p style=\"font-style: italic;\">Content text</p><p>second content</p>
            </div> <section class=\"sec\" /> "

ReadNode html