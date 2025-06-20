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
        (pkgs.runCommand "create-fish-session-vars" { } ''
          mkdir -p $out/share/fish
          ${lib.getExe pkgs.babelfish} \
            < ${config.home.sessionVariablesPackage}/etc/profile.d/hm-session-vars.sh \
            > $out/share/fish/hm-session-vars.fish
        '')
      ];
    };

    symlink-config.files = [
      {
        source = ./config.fish;
        destination = "fish/config.fish";
        xdg = true;
      }
      {
        source = ./conf.d;
        destination = "fish/conf.d";
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
