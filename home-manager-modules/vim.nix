{ config, lib, pkgs, ... }:

{
  programs.vim = {
    enable = false;
    settings = {
      background = "dark";
      copyindent = true;
      expandtab = true;
      number = true;
      relativenumber = true;
    };
    extraConfig = ''
      set encoding=utf-8
      set nowrap
      set showmatch
      set tabstop=4
      set softtabstop=4
      set shiftwidth=4
      set smartcase
      set backspace=indent,eol,start
      set showmode
      set showcmd
    '';
  };
}
