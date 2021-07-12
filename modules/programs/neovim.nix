{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.programs.neovim;
in {
  options.modules.programs.neovim = { enable = mkEnableOption "neovim"; };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.neovim-nightly ];
    nixpkgs.overlays = let
      rev = "8814fde215d6d34036c55135f33471a0edf07453";
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
