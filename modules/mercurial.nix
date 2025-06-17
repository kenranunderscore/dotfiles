{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.mercurial;
in
{
  options.my.mercurial = {
    enable = lib.mkEnableOption "mercurial";

    email = lib.mkOption {
      type = lib.types.str;
      default = "";
    };
  };

  config = {
    programs = {
      mercurial = {
        enable = true;
        userEmail = cfg.email;
        userName = "Johannes Maier";
        inherit (config.programs.git) ignores;
        aliases = {
          p = "pull -u";
        };
      };
    };
  };
}
