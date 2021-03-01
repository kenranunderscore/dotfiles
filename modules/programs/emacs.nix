{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.programs.emacs;
in {
  options.modules.programs.emacs = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    version = mkOption {
      type = types.enum [ "gcc" "unstable" "stable" ];
      default = "stable";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      (if cfg.version == "gcc" then
        pkgs.emacsGcc
      else
        (if cfg.version == unstable then emacsUnstable else emacs))
    ];
    nixpkgs.overlays = let
      rev = "208c2bd388eee9b58dd56dd4ba1db863fee1dcba";
      emacsOverlay = (import (builtins.fetchTarball {
        url =
          "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
      }));
    in [ emacsOverlay ];
  };
}
