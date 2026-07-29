; Hand indentation back to Vim across markdown. @indent.auto makes indentexpr
; return -1 ("keep the indent Vim already computed"), and Vim's 'autoindent'
; plus 'formatlistpat' align a wrapped list item under the item's own text
; rather than at column 0.
;
; Prose lines are governed by the companion markdown_inline query, not this
; one: indentation resolves against the innermost tree containing the line,
; and text inside a list item belongs to the injected markdown_inline tree.
;
; Fenced code is unaffected — an injected language uses its own indents.scm.
(document) @indent.auto
