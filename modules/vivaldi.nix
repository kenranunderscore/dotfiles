{
  config,
  lib,
  inputs,
  pkgs,
  ...
}:

{
  options.my.vivaldi = {
    enable = lib.mkEnableOption "vivaldi";

    wrapWithNixGL = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
  };

  config = lib.mkIf config.my.vivaldi.enable {
    home.packages =
      let
        vivaldi = pkgs.vivaldi.override { proprietaryCodecs = true; };
        vivaldi-wrapper =
          if config.my.vivaldi.wrapWithNixGL then
            inputs.self.lib.createNixGLWrapper pkgs vivaldi
          else
            vivaldi;
      in
      [ vivaldi-wrapper ];
  };
}
