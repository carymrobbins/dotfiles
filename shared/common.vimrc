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
"Similar to above except moves close paren to next line
nmap <Leader>0 f)i<CR><ESC>l
"Insert a newline without going into insert mode
nmap <Leader>o o<ESC>
"Append to end of lines of selection
xmap <Leader>a $A

"Helpers for revealjs hacks
"Insert fragment spans for markdown slides
vmap <Leader>ff c<frag><ESC>:set paste<CR>a<C-r>"</frag><ESC>:set nopaste<CR>
"Insert fragment divs for markdown slides
nmap <Leader>fd I<div class=fragment><CR><ESC>
"Insert code block for markdown slides
vmap <Leader>cb c<pre><code data-noescape data-trim class=scala><CR><C-r>"<CR></code></pre><ESC>
