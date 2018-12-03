execute pathogen#infect()

source ~/dotfiles/shared/common.vimrc

syntax on
filetype plugin on
filetype indent on

"Show line, column numbers in status bar.
set ruler
"Always show file name.
set ls=2
"Custom command to clear search highlighting.
command ClearSearch :let @/ = ""
"Look for tags starting with current directory traversing upwards.
set tags=./tags,tags,codex.tags;

"Use line numbers, color them grey.
set number
highlight LineNr ctermfg=grey

"Search highlighting color
"
"highlight Visual ctermbg=lightgrey ctermfg=darkgrey
"highlight Search ctermbg=lightgrey ctermfg=darkgrey

highlight Visual ctermbg=darkgrey ctermfg=lightgrey
highlight Search ctermbg=darkgrey ctermfg=lightgrey
"Enable backspace.
set backspace=indent,eol,start
"Tab settings.
set smartindent
set tabstop=8     "A tab is 8 spaces
set expandtab     "Always uses spaces instead of tabs
set softtabstop=2 "Insert 4 spaces when tab is pressed
set shiftwidth=2  "An indent is 4 spaces
set smarttab      "Indent instead of tab at start of line
set shiftround    "Round spaces to nearest shiftwidth multiple
set nojoinspaces  "Don't convert spaces to tabs

"Key mappings
"""""""""""""
"Copy selection to system clipboard
xmap <Leader>y "+y
"Paste system clipboard
nmap <Leader>p "+p
"Toggle spellchecker
nmap <Leader>s :setlocal spell!<CR>
"Toggle line numbers
nmap <Leader>n :setlocal number!<CR>
"Toggle hlsearch
nmap <Leader>h :setlocal hlsearch!<CR>
"Toggle color column (max line length bar)
nmap <Leader>c :call ToggleColorColumn()<CR>
"Ctrl+N to open/close NERDTree
map <C-n> :NERDTreeToggle<CR>
"Remap <Enter> to split the line and insert a new line in between for braces/parens
inoremap <expr> <CR> BreakLine() ? "<CR><ESC>O" : "<CR>"
"Redraw the screen, useful for fixing bad highlighting
nmap <Leader>r :redraw!<CR>

"Customize for solarized dark/light

"TODO: FIXME colorscheme solarized
"Store and read background state from a file
let g:backgroundFile = expand("~/.vim/background.vim")
if filereadable(g:backgroundFile)
  execute 'source '.fnameescape(g:backgroundFile)
endif

fun Bgd()
  silent execute '!echo "set background=dark" > '.fnameescape(g:backgroundFile)
  set background=dark
endfun

com Bgd :call Bgd()

fun Bgl()
  silent execute '!echo "set background=light" > '.fnameescape(g:backgroundFile)
  set background=light
endfun

com Bgl :call Bgl()

"Highlighting for custom file types

au BufNewFile,BufRead kwmrc set filetype=c
au BufNewFile,BufRead .skhdrc set filetype=python
au BufNewFile,BufRead *.json set filetype=javascript
au BufNewFile,BufRead xmobarrc set filetype=haskell
au BufNewFile,BufRead .ghci set filetype=haskell
au BufNewFile,BufRead *.zsh-theme set filetype=zsh
au BufNewFile,BufRead *.x set filetype=alex

"Prevent YouCompleteMe scratch preview from staying open.
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

"BreakLine: Return TRUE if in the middle of {} or () in INSERT mode
fun BreakLine()
  if (mode() == 'i')
    return ((getline(".")[col(".")-2] == '{' && getline(".")[col(".")-1] == '}') ||
          \(getline(".")[col(".")-2] == '(' && getline(".")[col(".")-1] == ')'))
  else
    return 0
  endif
endfun

"Open NERDTree automatically if no files specified when opened.
autocmd vimenter * if !argc() | NERDTree | endif
"Show hidden files.
let NERDTreeShowHidden=1

"Close if NERDTree is the only open window.
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

let g:syntastic_python_checkers = ['pylint']

"Highlight trailing whitespace
match Todo /\s\+$/

fun ToggleColorColumn()
  if (&colorcolumn > 0)
    set colorcolumn=0
  else
    set colorcolumn=101
  endif
endfun

" Disable auto-folding in markdown files
let g:vim_markdown_folding_disabled=1

" Disable haskell-vim auto-indentation
let g:haskell_indent_disable=1

" Use :TrimWhitespace to...trim the trailing whitespace.
fun! TrimWhitespace()
    let l:save = winsaveview()
    %s/\s\+$//e
    call winrestview(l:save)
endfun

command! TrimWhitespace call TrimWhitespace()
