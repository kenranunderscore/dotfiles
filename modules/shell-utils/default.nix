{ pkgs, ... }:

{
  home = {
    packages = [ pkgs.perl ];
    file = {
      ".local/bin/select_project.pl".source = ./select_project.pl;
    };
  };
}
