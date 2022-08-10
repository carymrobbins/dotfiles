" Adapted from https://github.com/aliou/sql-heredoc.vim/blob/master/after/syntax/ruby.vim

" SQL interpolation
let s:previous_syntax = b:current_syntax
unlet b:current_syntax
syntax include @SQL syntax/sql.vim
syntax region shHereDocSQL matchgroup=Statement start=+<<'\?\z([A-Z]*SQL[A-Z]*\)'\?+ end=+^\z1$+ contains=@SQL,shSQLInterpolation
let b:current_syntax = s:previous_syntax

" JSON interpolation
let s:previous_syntax = b:current_syntax
unlet b:current_syntax
syntax include @JSON syntax/json.vim
syntax region shHereDocJSON matchgroup=Statement start=+<<'?\z([A-Z]*JSON[A-Z]*\)'\?+ end=+^\z1$+ contains=@JSON,shJSONInterpolation
let b:current_syntax = s:previous_syntax

" NIX interpolation
let s:previous_syntax = b:current_syntax
unlet b:current_syntax
syntax include @NIX syntax/sql.vim
syntax region shHereDocNIX matchgroup=Statement start=+<<'\?\z([A-Z]*NIX[A-Z]*\)'\?+ end=+^\z1$+ contains=@NIX,shNIXInterpolation
let b:current_syntax = s:previous_syntax
