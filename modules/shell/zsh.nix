{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.zsh;
in {
  options.modules.shell.zsh = { enable = mkEnableOption "zsh"; };

  config = mkIf cfg.enable {
    home.file = {
      ".zshrc".source = ../../config/zshrc;
      ".zshenv".source = ../../config/zshenv;
    };
    programs.zsh.enable = true;
  };
}
