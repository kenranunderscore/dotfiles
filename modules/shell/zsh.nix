{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.zsh;
in {
  options.modules.shell.zsh = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home.file = {
      ".zshrc".source = ../../config/zshrc;
      ".zshenv".source = ../../config/zshenv;
    };
    programs.zsh.enable = true;
  };
}
