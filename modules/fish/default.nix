{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.fish.enable = lib.mkEnableOption "fish";

  config = lib.mkIf config.my.fish.enable {
    home = {
      packages = [
        pkgs.fish
        pkgs.eza
      ];
      file.".config/fish/conf.d/hm-session-vars.fish".source =
        pkgs.runCommand "create-fish-session-vars" { }
          ''
            ${lib.getExe pkgs.babelfish} \
              < ${config.home.sessionVariablesPackage}/etc/profile.d/hm-session-vars.sh \
              > $out
          '';
    };

    symlink-config.files = [
      {
        source = ./config.fish;
        destination = "fish/config.fish";
        xdg = true;
      }
      {
        source = ./functions;
        destination = "fish/functions";
        xdg = true;
      }
    ];
  };
}
