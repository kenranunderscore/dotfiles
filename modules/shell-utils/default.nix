{ pkgs, ... }:

{
  symlink-config.files = [
    {
      source = ./select_project.pl;
      destination = ".local/bin/select_project.pl";
    }
  ];

  home.packages = [ pkgs.perl ];
}
