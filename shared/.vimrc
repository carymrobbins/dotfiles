execute pathogen#infect()

source ~/dotfiles/shared/common.vimrc

syntax on
filetype plugin on
filetype indent on

"Default to nowrap, toggle with <Leader>W (see common.vimrc)
set nowrap
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

"Remap j and k to move via visual lines (respecting wrapping)
nnoremap j gj
nnoremap k gk

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

"Also deals with vim-tmux-navigator
fun! Automax()
  execute "normal \<C-w>_"
  nnoremap <C-w>j <C-w>j<C-w>_
  nnoremap <C-w>k <C-w>k<C-w>_
  if exists("g:loaded_tmux_navigator")
    nnoremap <silent> <c-h> :TmuxNavigateLeft<cr><C-w>_
    nnoremap <silent> <c-j> :TmuxNavigateDown<cr><C-w>_
    nnoremap <silent> <c-k> :TmuxNavigateUp<cr><C-w>_
    nnoremap <silent> <c-l> :TmuxNavigateRight<cr><C-w>_
    nnoremap <silent> <c-\> :TmuxNavigatePrevious<cr><C-w>_
  endif
endfun

command! Automax call Automax()

"Also deals with vim-tmux-navigator
fun! Noautomax()
  execute "normal \<C-w>="
  nnoremap <C-w>j <C-w>j
  nnoremap <C-w>k <C-w>k
  if exists("g:loaded_tmux_navigator")
    nnoremap <silent> <c-h> :TmuxNavigateLeft<cr>
    nnoremap <silent> <c-j> :TmuxNavigateDown<cr>
    nnoremap <silent> <c-k> :TmuxNavigateUp<cr>
    nnoremap <silent> <c-l> :TmuxNavigateRight<cr>
    nnoremap <silent> <c-\> :TmuxNavigatePrevious<cr>
  endif
endfun

command! Noautomax call Noautomax()

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
au BufNewFile,BufRead *.json set filetype=json
au BufNewFile,BufRead xmobarrc set filetype=haskell
au BufNewFile,BufRead .ghci set filetype=haskell
au BufNewFile,BufRead *.hsfiles set filetype=haskell
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
"autocmd vimenter * if !argc() | NERDTree | endif

"Show hidden files.
let NERDTreeShowHidden=1

"Close if NERDTree is the only open window.
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

"Enable indent guides with <Leader>ig
"https://github.com/nathanaelkane/vim-indent-guides/issues/131#issuecomment-389757643
highlight Normal ctermbg=NONE
"Make indent guides skinny
let g:indent_guides_guide_size = 1
"Customize indent guide colors
let g:indent_guides_auto_colors = 0
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  ctermbg=255
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=253

let g:syntastic_python_checkers = ['pylint']
let g:vim_json_syntax_conceal = 0

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

" Color test: Save this file, then enter ':so %'
" Then enter one of following commands:
"   :VimColorTest    "(for console/terminal Vim)
"   :GvimColorTest   "(for GUI gvim)
" Adapted from https://vim.fandom.com/wiki/View_all_colors_available_to_gvim
" but changed to support 255 bg colors and always display fg as black
function! VimColorTest(outfile, fgend, bgend)
  let result = []
  for fg in range(1)
    for bg in range(255)
      let kw = printf('%-7s', printf('c_%d_%d', fg, bg))
      let h = printf('hi %s ctermfg=%d ctermbg=%d', kw, fg, bg)
      let s = printf('syn keyword %s %s', kw, kw)
      call add(result, printf('%-32s | %s', h, s))
    endfor
  endfor
  call writefile(result, a:outfile)
  execute 'edit '.a:outfile
  source %
endfunction
" Increase numbers in next line to see more colors.
command! VimColorTest call VimColorTest('vim-color-test.tmp', 12, 16)
