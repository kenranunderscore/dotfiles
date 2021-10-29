{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.zsh;
in {
  options.modules.shell.zsh.enable = lib.mkEnableOption "zsh";

  config = lib.mkIf cfg.enable {
    home.file = {
      ".zshrc".source = ../../config/zshrc;
      ".zshenv".source = ../../config/zshenv;
    };
    programs.zsh.enable = true;
  };
}
