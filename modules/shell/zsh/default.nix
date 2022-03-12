{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.zsh;
in {
  options.modules.shell.zsh.enable = lib.mkEnableOption "zsh";

  config = lib.mkIf cfg.enable {
    home.file = {
      ".zshrc".source = ./zshrc;
      ".zshenv".source = ./zshenv;
    };
    programs.zsh.enable = true;
  };
}
