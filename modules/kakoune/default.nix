{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.kakoune.enable = lib.mkEnableOption "kakoune";

  config = lib.mkIf config.my.kakoune.enable {
    symlink-config.files = [
      {
        source = ./kakrc;
        destination = "kak/kakrc";
        xdg = true;
      }
    ];

    home = {
      activation.installKakounePlugins = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p "$XDG_CONFIG_HOME/kak/bundle"
        dest="$XDG_CONFIG_HOME/kak/bundle/kak-bundle"
        if [ ! -d "$dest" ]; then
          $DRY_RUN_CMD \
            ${lib.getExe pkgs.gitMinimal} clone \
            https://github.com/jdugan6240/kak-bundle \
            "$dest"
        fi
      '';
      packages =
        let
          k = pkgs.writeShellScriptBin "k" ''
            if [ -z "$TMUX" ]; then
              exec tmux new -As kak kak "$@"
            else
              exec kak "$@"
            fi
          '';
        in
        [
          k
          pkgs.kakoune
          pkgs.kak-lsp
          pkgs.kak-tree-sitter
        ];
    };
  };
}
