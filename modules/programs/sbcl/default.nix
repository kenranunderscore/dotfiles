{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.zsh;
in {
  options.modules.programs.sbcl.enable = lib.mkEnableOption "sbcl";

  config = lib.mkIf cfg.enable {
    home = {
      file.".sbclrc".source = ./sbclrc;
      packages = [ pkgs.sbcl ];
    };
  };
}
