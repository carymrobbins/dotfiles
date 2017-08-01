fun s:DetectCustomLang()
  if getline(1) =~# '^#!.*\<\(haskript\|runhaskell\|runghc\)\>'
    setfiletype haskell
  endif
endfun

autocmd BufNewFile,BufRead * call s:DetectCustomLang()
