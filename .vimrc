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

" vim-plug config

filetype off

call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'vim-scripts/conque-gdb'
Plug 'jiangmiao/auto-pairs'
Plug 'morhetz/gruvbox'
Plug 'kien/ctrlp.vim'
Plug 'rust-lang/rust.vim'
Plug 'lukerandall/haskellmode-vim'
Plug 'eagletmt/ghcmod-vim'
Plug 'scrooloose/syntastic'
Plug 'ujihisa/neco-ghc'
Plug 'kovisoft/slimv'
Plug 'shougo/vimproc', {'do' : 'make'}

call plug#end()

" Custom config

set termguicolors
set t_8f=[38;2;%lu;%lu;%lum
set t_8b=[48;2;%lu;%lu;%lum

syntax on

set colorcolumn=80

colorscheme gruvbox
set background=dark

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

" Different command mode shortcuts

inoremap jk <ESC>
inoremap ht <ESC>

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
