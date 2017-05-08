set noswapfile
syntax enable

" ----------------------------------------------------------------------------
"  Text Formatting
" ----------------------------------------------------------------------------

set autoindent             " automatic indent new lines
set smartindent            " be smart about it
inoremap # X<BS>#
set wrap                   " wrap lines
set softtabstop=2          " yep, two
set shiftwidth=2           " ..
set tabstop=4
set expandtab              " expand tabs to spaces
set nosmarttab             " fuck tabs
set formatoptions+=n       " support for numbered/bullet lists
"set textwidth=80           " wrap at 80 chars by default
set virtualedit=block      " allow virtual edit in visual block ..

" ----------------------------------------------------------------------------
"  Remapping
" ----------------------------------------------------------------------------

" lead with ,
let mapleader = ","

" exit to normal mode with 'jj'
inoremap jj <ESC>


" reflow paragraph with Q in normal and visual mode
nnoremap Q gqap
vnoremap Q gq

" sane movement with wrap turned on
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
nnoremap <Down> gj
nnoremap <Up> gk
vnoremap <Down> gj
vnoremap <Up> gk
inoremap <Down> <C-o>gj
inoremap <Up> <C-o>gk

" ----------------------------------------------------------------------------
"  UI
" ----------------------------------------------------------------------------

set ruler                  " show the cursor position all the time
set noshowcmd              " don't display incomplete commands
set nolazyredraw           " turn off lazy redraw
set number                 " line numbers
set wildmenu               " turn on wild menu
set wildmode=list:longest,full
set ch=2                   " command line height
set backspace=2            " allow backspacing over everything in insert mode
set whichwrap+=<,>,h,l,[,] " backspace and cursor keys wrap to
set shortmess=filtIoOA     " shorten messages
set report=0               " tell us about changes
set nostartofline          " don't jump to the start of line when scrolling


" ----------------------------------------------------------------------------
" Visual Cues
" ----------------------------------------------------------------------------

set showmatch              " brackets/braces that is
set mat=5                  " duration to show matching brace (1/10 sec)
set incsearch              " do incremental searching
set laststatus=2           " always show the status line
set ignorecase             " ignore case when searching
set nohlsearch             " don't highlight searches
set visualbell             " shut the fuck up


" ---------------------------------------------------------------------------
"  Strip all trailing whitespace in file
" ---------------------------------------------------------------------------

function! StripWhitespace ()
    exec ':%s/ \+$//gc'
endfunction
map ,s :call StripWhitespace ()<CR>

" ---------------------------------------------------------------------------
" Edit gpg-encrypted ascii-armoured files
" ---------------------------------------------------------------------------
autocmd! BufReadPre,FileReadPre      *.asc  set bin
autocmd  BufReadPost,FileReadPost    *.asc  '[,']!gpg -q -d
autocmd  BufReadPost,FileReadPost    *.asc  set nobin
autocmd! BufWritePre,FileWritePre    *.asc  set bin
autocmd  BufWritePre,FileWritePre    *.asc  '[,']!gpg -e
autocmd  BufWritePost,FileWritePost  *.asc  undo
autocmd  BufWritePost,FileWritePost  *.asc  set nobin

autocmd! BufReadPre,FileReadPre      *.gpg  set bin
autocmd  BufReadPost,FileReadPost    *.gpg  '[,']!gpg -q -d
autocmd  BufReadPost,FileReadPost    *.gpg  set nobin
autocmd! BufWritePre,FileWritePre    *.gpg  set bin
autocmd  BufWritePre,FileWritePre    *.gpg  '[,']!gpg -e
autocmd  BufWritePost,FileWritePost  *.gpg  undo
autocmd  BufWritePost,FileWritePost  *.gpg  set nobin

set diffexpr=MyDiff()
function MyDiff()
  let opt = ""
  if &diffopt =~ "iwhite"
    let opt = opt . "-b "
  endif
  silent execute "!git-diff-normal-format " . opt . v:fname_in . " " . v:fname_new . " > " . v:fname_out
  redraw!
endfunction
