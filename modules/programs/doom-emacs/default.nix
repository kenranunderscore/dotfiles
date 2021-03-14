{ config, lib, pkgs, ... }:

with lib;
with import <home-manager/modules/lib/dag.nix> { inherit lib; };
let
  cfg = config.modules.programs.doomEmacs;
  configPath = builtins.toPath ./.;
in {
  options.modules.programs.doomEmacs = {
    enable = mkEnableOption "doomEmacs";

    emacsVersion = mkOption {
      type = types.enum [ "gcc" "unstable" "stable" ];
      default = "stable";
    };
  };

  config = mkIf cfg.enable {
    home = {
      activation = {
        # FIXME Check for existence of ~/.emacs.d
        symlinkAndSyncDoom = dagEntryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD ln -snf ${configPath}/doom $HOME/.config/doom && \
          $DRY_RUN_CMD ln -snf ${configPath}/.emacs.d $HOME/.emacs.d && \
          $DRY_RUN_CMD ~/.emacs.d/bin/doom sync
        '';
      };
      packages = [
        (if cfg.emacsVersion == "gcc" then
          pkgs.emacsGcc
        else
          (if cfg.emacsVersion == "unstable" then
            pkgs.emacsUnstable
          else
            pkgs.emacs))
      ];
    };
    nixpkgs.overlays = let
      rev = "e35ed9bf10b46e533e52add965926d00122c0620";
      emacsOverlay = (import (builtins.fetchTarball {
        url =
          "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
      }));
    in [ emacsOverlay ];
  };
}
