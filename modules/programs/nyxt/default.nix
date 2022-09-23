{ config, lib, pkgs, ... }:

let
  cfg = config.modules.programs.nyxt;

  # To get the most recent version I build Nyxt from source locally.
  # This wrapper script assumes that the repo is cloned at
  # ~/projects/forks/nyxt.
  nyxtWrapped = pkgs.writeShellScriptBin "nyxt" ''
    export WEBKIT_DISABLE_COMPOSITING_MODE=1
    nix-shell ~/projects/forks/nyxt/build-scripts/shell.nix --run "~/projects/forks/nyxt/nyxt $@"
  '';
in {
  options.modules.programs.nyxt.enable = lib.mkEnableOption "nyxt";

  config = lib.mkIf cfg.enable {
    home = {
      packages = [ nyxtWrapped ];
      activation.symlinkNyxtConfig =
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          config_file=$HOME/.config/nyxt/config.lisp

          if [ ! -e "$config_file" ]; then
            mkdir -p "$(dirname "$config_file")"
            $DRY_RUN_CMD ln -s $HOME/dotfiles/modules/programs/nyxt/config.lisp "$config_file"
          fi
        '';
    };
  };
}
