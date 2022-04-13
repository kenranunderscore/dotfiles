{ config, lib, pkgs, ... }:

let
  cfg = config.modules.programs.nyxt;

  # To get the most recent version I build Nyxt from source locally.
  # This wrapper script assumes that the repo is cloned at
  # ~/projects/forks/nyxt.
  nyxtWrapped = pkgs.writeShellScriptBin "nyxt"
    "nix-shell ~/projects/forks/nyxt/build-scripts/shell.nix --run '~/projects/forks/nyxt/nyxt'";
in {
  options.modules.programs.nyxt.enable = lib.mkEnableOption "nyxt";

  config = lib.mkIf cfg.enable { home.packages = [ nyxtWrapped ]; };
}
