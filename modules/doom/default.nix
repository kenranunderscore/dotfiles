{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.doom;
in
{
  options.my.doom = {
    enable = lib.mkEnableOption "doom";
    includePkg = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    symlink-config.files = [
      {
        source = ./doom;
        destination = "$XDG_CONFIG_HOME/doom";
      }
    ];

    home = {
      activation.cloneDoomEmacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
          $DRY_RUN_CMD ${lib.getExe pkgs.gitMinimal} clone \
                       --depth=1 \
                       --single-branch \
                       https://github.com/doomemacs/doomemacs \
                       "$XDG_CONFIG_HOME/emacs"
        fi
      '';

      packages =
        let
          emacsWithPackages = (pkgs.emacsPackagesFor pkgs.emacs30).emacsWithPackages;
          # Doom manages packages itself, but vterm is an exception as it
          # sometimes does not build in a naive way. Also have Emacs know all
          # treesit grammars by default, so we don't have to install them
          # externally later on.
          myEmacs = emacsWithPackages (p: [
            p.vterm
            p.treesit-grammars.with-all-grammars
            p.mu4e
          ]);
        in
        with pkgs;
        lib.optionals cfg.includePkg [
          myEmacs

          # Programs needed at runtime
          cmake
          fd
          libtool
          meson
          ninja
          ripgrep
          shellcheck
        ];
    };
  };
}
