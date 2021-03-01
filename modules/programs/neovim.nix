{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.programs.neovim;
in {
  options.modules.programs.neovim = { enable = mkEnableOption "neovim"; };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.neovim-nightly ];
    nixpkgs.overlays = let
      rev = "cf6e8a71c34b6dea741f920f9e6fe4b5328fb2b5";
      neovimOverlay = import (builtins.fetchTarball {
        url =
          "https://github.com/nix-community/neovim-nightly-overlay/archive/${rev}.tar.gz";
      });
    in [ neovimOverlay ];
    xdg.configFile = {
      "nvim" = {
        source = ../../config/nvim;
        recursive = true;
      };
    };
  };
}
