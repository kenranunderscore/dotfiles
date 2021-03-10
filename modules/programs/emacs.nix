{ config, lib, pkgs, ... }:

with lib;
with import <home-manager/modules/lib/dag.nix> { inherit lib; };
let
  cfg = config.modules.programs.emacs;
  configPath = builtins.toPath ../../config;
in {
  options.modules.programs.emacs = {
    enable = mkEnableOption "emacs";

    version = mkOption {
      type = types.enum [ "gcc" "unstable" "stable" ];
      default = "stable";
    };
  };

  config = mkIf cfg.enable {
    home = {
      activation = {
        symlinkAndSyncDoom = dagEntryAfter [ "writeBoundary" ] ''
          # FIXME Check for existence of ~/.emacs.d
          $DRY_RUN_CMD ln -snf ${configPath}/doom $HOME/.config/doom && \
          $DRY_RUN_CMD ln -snf ${configPath}/doom-emacs $HOME/.emacs.d && \
          $DRY_RUN_CMD ~/.emacs.d/bin/doom sync
        '';
      };
      packages = [
        (if cfg.version == "gcc" then
          pkgs.emacsGcc
        else
          (if cfg.version == "unstable" then
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
