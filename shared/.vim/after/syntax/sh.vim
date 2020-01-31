" Adapted from https://github.com/aliou/sql-heredoc.vim/blob/master/after/syntax/ruby.vim

" Store current syntax.
let s:previous_syntax = b:current_syntax

" Store the SQL syntax so it can be included below.
unlet b:current_syntax
syntax include @SQL syntax/sql.vim

syntax region shHereDocSQL matchgroup=Statement start=+<<\z([A-Z]*SQL[A-Z]*\)+ end=+^\z1$+ contains=@SQL,shInterpolation

let b:current_syntax = s:previous_syntax
