{ config, lib, pkgs, ... }:

let cfg = config.modules.programs.neovim;
in {
  options.modules.programs.neovim.enable = lib.mkEnableOption "neovim";

  config = lib.mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        neovim-nightly
        lua
        sumneko-lua-language-server
        stylua
        xclip
      ];

      activation = {
        symlinkNeovimConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          if [ ! -h $HOME/.config/nvim ]; then
              $DRY_RUN_CMD mkdir -p $HOME/.config
              $DRY_RUN_CMD ln -snf $HOME/dotfiles/home-manager-modules/programs/neovim/nvim $HOME/.config/nvim
          fi
        '';
      };
    };
  };
}
