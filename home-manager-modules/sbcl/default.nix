{ config, lib, pkgs, ... }:

let cfg = config.modules.sbcl;
in {
  options.modules.sbcl.enable = lib.mkEnableOption "sbcl";

  config = lib.mkIf cfg.enable {
    home = {
      file.".sbclrc".source = ./sbclrc;
      packages = [ pkgs.sbcl ];
    };
  };
}
