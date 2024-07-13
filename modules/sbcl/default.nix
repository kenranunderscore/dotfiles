{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.sbcl;
in
{
  options.modules.sbcl = {
    enable = lib.mkEnableOption "sbcl";

    withPackage = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    home = {
      file.".sbclrc".source = ./sbclrc;
      packages = lib.optional cfg.withPackage pkgs.sbcl;
    };
  };
}
