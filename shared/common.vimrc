""""""""""""""""""""""""""""""""""""""""""""""
" Common vimrc used by .vimrc and .ideavimrc "
""""""""""""""""""""""""""""""""""""""""""""""

"Set leader to spacebar
let mapleader=" "

"Highlight search terms.
set hlsearch
" Disable bell in IntelliJ when pressing esc
set visualbell
set noerrorbells

" Map // to search for visually selected text.
vnoremap // y/<C-R>"<CR>

"Key mappings
"""""""""""""
"Save current file
nmap <Leader>w :w<CR>
"Save and close current file
nmap <Leader>x :x<CR>
"Close current file
nmap <Leader>q :q<CR>
"Inverse of J
nmap <Leader>j f xi<CR><ESC>l
"Insert a newline without going into insert mode
nmap <Leader>o o<ESC>
"Append to end of lines of selection
xmap <Leader>a $A

"Greek letters
inoremap <C-a> α
inoremap <C-b> β
inoremap <C-g> γ
inoremap <C-d> δ
inoremap <C-e> ϵ
inoremap <C-l> λ
inoremap <C-k> Λ
