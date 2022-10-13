{ config, lib, pkgs, ... }:

let cfg = config.modules.programs.neovim;
in {
  options.modules.programs.neovim.enable = lib.mkEnableOption "neovim";

  config = lib.mkIf cfg.enable {
    programs.neovim = {
      enable = true;
      package = pkgs.neovim-nightly;
      plugins = with pkgs.vimPlugins; [
        auto-pairs
        indentLine
        lightline-vim
        neoformat
        nvim-compe
        telescope-nvim
        vim-fugitive
        vim-lion
        vim-nix
        vim-rhubarb
        vim-rooter
        vim-surround
        vim-sleuth
        vim-sneak
      ];
    };
  };
}
