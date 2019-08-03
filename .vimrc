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

" Plugins

filetype off

if empty(glob('~/.vim/autoload/plug.vim'))
    echo "hi"
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
           \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

" Color schemes
Plug 'chriskempson/base16-vim'

" NerdTree
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'

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
colorscheme base16-atelier-estuary

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
