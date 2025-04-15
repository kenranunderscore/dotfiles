{
  config,
  lib,
  pkgs,
  ...
}:

{
  config = {
    home = {
      packages = [ pkgs.perl ];
      file = {
        ".local/bin/navigate_to_project.pl".source = ./navigate_to_project.pl;
      };
    };
  };
}
