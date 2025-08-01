{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.bash;
in
{
  options.my.bash.enable = lib.mkEnableOption "bash";

  config = lib.mkIf cfg.enable {
    programs.bash = {
      enable = true;
      historyIgnore = [
        "ls"
        "cd"
        "exit"
      ];
      shellAliases = import ./shell-aliases.nix { inherit pkgs; };
      historyControl = [
        "ignorespace"
        "ignoredups"
        "erasedups"
      ];
      profileExtra = ''
        export PATH="''$HOME/.local/bin:''$PATH"
      '';
    };
  };
}
