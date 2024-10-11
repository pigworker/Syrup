import {EditorView, basicSetup} from "codemirror"
import {syrup} from "./lang-syrup"

import {tags} from "@lezer/highlight"
import {HighlightStyle, syntaxHighlighting} from "@codemirror/language"

const myHighlightStyle = HighlightStyle.define([
  {tag: tags.className, color: "blue"},
  {tag: tags.typeName, color: "forestgreen"},
  {tag: tags.lineComment, color: "#c71"},
  {tag: tags.operatorKeyword, color: "darkslateblue"},
  {tag: tags.variableName, color: "sienna", fontStyle: "italic"},
])

let editor = new EditorView({
    extensions: [basicSetup, syrup(),syntaxHighlighting(myHighlightStyle)],
  parent: document.body
})
