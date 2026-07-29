; Text inside a list item lives in this injected tree, so this is the query that
; actually governs indentation when gq reflows prose. @indent.auto returns -1 so
; Vim's own 'formatlistpat' handling aligns wrapped lines under the list item's
; text. See the companion markdown query for the full explanation.
(inline) @indent.auto
