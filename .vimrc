" Use vim settings

set nocompatible

" Some gVim specific settings

if has("gui_running")
    set langmenu=en_US.UTF-8
    set lines=999 columns=999
    set guioptions-=m
    set guioptions-=T
    set guioptions-=r
    set guicursor+=i-ci:block-Cursor/lCursor
    set guifont=Fira\ Code\ 11
endif

" Vundle config

filetype off

call plug#begin('~/.vim/plugged')

" Color schemes
Plug 'morhetz/gruvbox'
Plug 'nightsense/snow'
Plug 'ayu-theme/ayu-vim'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'mhartington/oceanic-next'
Plug 'joshdick/onedark.vim'

" NerdTree
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'

" Clojure
Plug 'tpope/vim-salve'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fireplace'
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'kien/rainbow_parentheses.vim'
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-clojure-highlight'
Plug 'tpope/vim-surround'

" Rust
Plug 'rust-lang/rust.vim'

" Haskell
Plug 'neovimhaskell/haskell-vim'

" General
Plug 'scrooloose/syntastic'
Plug 'tomtom/tlib_vim'
Plug 'marcweber/vim-addon-mw-utils'
Plug 'godlygeek/tabular'
Plug 'ervandew/supertab'
Plug 'shougo/neocomplete.vim'
Plug 'kien/ctrlp.vim'
Plug 'shougo/vimproc', {'do' : 'make'}
Plug 'jeffkreeftmeijer/vim-numbertoggle'

call plug#end()

" Custom config

if exists('+termguicolors')
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif

syntax enable

"set colorcolumn=80

set background=dark
colorscheme onedark

set encoding=utf-8
set nowrap
set showmatch
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set autoindent
set copyindent
set smartcase
set backspace=indent,eol,start
set showmode
set showcmd
set number
set relativenumber

" Different command mode shortcut

inoremap jk <ESC>

" Different mapleader

let mapleader=","

" Disable arrow keys

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

" DCSS level design syntax highlighting

au BufRead,BufNewFile *.des set syntax=levdes

" Haskell

let g:haddock_browser="google-chrome-stable"

let g:haskell_tabular = 1
vmap a= :Tabularize /=<CR>
vmap a; :Tabularize /::<CR>
vmap a- :Tabularize /-><CR>

autocmd VimEnter * RainbowParenthesesToggle
autocmd Syntax   clojure RainbowParenthesesLoadRound
autocmd Syntax   clojure RainbowParenthesesLoadSquare
autocmd Syntax   clojure RainbowParenthesesLoadBraces

autocmd BufRead *.clj try | silent! Require | catch /^Fireplace/ | endtry

