" vim: fdm=marker ts=2 sts=2 sw=2 fdl=0

" detect OS {{{
  let s:is_windows = has('win32') || has('win64')
  let s:is_cygwin = has('win32unix')
  let s:is_macvim = has('gui_macvim')
"}}}

set nocompatible
if s:is_windows
  set rtp+=~/.vim
endif

" Vim Plug {{{
call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree' " directory tree
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-surround' " for cs ys ds commands
Plug 'tpope/vim-fugitive' " git wrapper
Plug 'tpope/vim-repeat'   " To repeat plugin maps
Plug 'bling/vim-airline'
Plug 'altercation/vim-colors-solarized' " colorscheme
Plug 'latex-box-team/latex-box'
Plug 'kien/ctrlp.vim'
Plug 'majutsushi/tagbar' " Class outliner
Plug 'airblade/vim-gitgutter'
"Plug 'valloric/youcompleteme'
Plug 'easymotion/vim-easymotion'
Plug 'jceb/vim-orgmode'
Plug 'skywind3000/asyncrun.vim'

" Add plugins to &runtimepath
call plug#end()
"}}}

filetype plugin indent on

" Syntax Highlight
syntax on
colorscheme morning
"colorscheme torte

" Display
set nonumber     " Line Numbers
set scrolloff=3 " Always keep 3 lines below or above cursor
set showmatch   " Briefly Jumps to matching parens
set matchtime=3 " Matching Time

set listchars=eol:$,tab:>-,trail:$
set nolist
"set display+=lastline

" set textwidth=78   " Auto wrap
" set showbreak=\\\\

" Status line
set ruler        " Status Line line-col %
set laststatus=2 " All window will have a statusline
set history=50   " Cmdline history
set showcmd
set showmode
set wildmenu    " Cmd-Line Completion expands in statusline

" Tabs
set tabstop=4
set shiftwidth=4
set expandtab
set smarttab " Rewinds sw
"set softtabstop=2
set autoindent " Auto indent for files without type

" Search rules
set smartcase
set ignorecase
set hlsearch
set incsearch

" Auto write and autoread
set autowrite " Autosave on buffer switch
set autoread  " Good for compatibility with other IDE like VS, Perforce, etc.
set backup
set backupdir=~/.vimcache/backup
set directory=~/.vimcache/swap

" Completion settings
set complete-=i " Blocks scans of included folders.
                " TODO: See if it's still needed with neo-AWESOME-plugins
set showfulltag

set clipboard=unnamed " Automatically yanks/fetch MS Clipboard
set nrformats-=octal " always assume decimal numbers
set grepprg=grep\ -nH\ $* " Customize grep
set backspace=indent,eol,start " Backspace behavior

"if s:is_windows && !s:is_cygwin
"    " ensure correct shell in gvim
"    set shell=c:\windows\system32\cmd.exe
"    " TODO: Maybe switch it to MSYS
"endif

" Remaps <leader> char to Spacebar
nnoremap <Space> <nop>
let mapleader=" "
let g:mapleader=" "
let maploadlleader=" "
let g:maploadlleader=" "

""" Fugitive + asyncrun.vim cooperation
command! -bang -nargs=* -complete=file Make AsyncRun -program=make @ <args>

""""""" Function

function! DeleteHiddenBuffers()
    let tpbl=[]
    call map(range(1, tabpagenr('$')), 'extend(tpbl, tabpagebuflist(v:val))')
    for buf in filter(range(1, bufnr('$')), 'bufexists(v:val) && index(tpbl, v:val)==-1')
        silent execute 'bwipeout' buf
    endfor
endfunction

""""""" Keyboard Mapping
nnoremap <Leader>tt :NERDTreeToggle<CR>

" Window Nav Control
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l


" Vimgrep
nnoremap <Leader>va :vimgrep <cword> **<CR>
nnoremap <Leader>vc :vimgrep <cword> % <CR>

nnoremap <Leader>wc <c-w>c
nnoremap <Leader>ws <c-w>s

" Function Keys
map <F3> :set invlist<CR>
imap <F3> <Esc>:set invlist<CR>a
map <F4> :nohls<CR>
imap <F4> <Esc>:nohls<CR>a
map <silent> <F8> :if g:colors_name == "torte"<CR>colorscheme morning<CR>else<CR>colorscheme torte<CR>endif<CR>
imap <silent> <F8> <Esc>:if g:colors_name == "torte"<CR>colorscheme morning<CR>else<CR>colorscheme torte<CR>endif<CR>a
