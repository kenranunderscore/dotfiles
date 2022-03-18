{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.zsh;
in {
  options.modules.shell.zsh.enable = lib.mkEnableOption "zsh";

  config = lib.mkIf cfg.enable {
    # TODO(Johannes):
    #
    # - zsh-abbr
    # - ls colored
    # - prompt
    # - plugins from old zshrc
    # - things from old zshenv perhaps
    # - environment link /share/zsh?
    # 
    # home.file = {
    #   ".zshrc".source = ./zshrc;
    #   ".zshenv".source = ./zshenv;
    # };
    programs.zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;
      history = {
        size = 50000;
        share = true;
        expireDuplicatesFirst = true;
        ignoreDups = true;
        ignorePatterns = [ "rm *" "kill *" "pkill *" ];
      };
    };
  };
}
