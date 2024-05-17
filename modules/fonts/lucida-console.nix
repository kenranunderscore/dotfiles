{
  runCommand,
  fetchzip,
  pkgs,
}:

let
  font = fetchzip {
    url = "https://www.dafontfree.io/download/lucida-console/?wpdmdl=71987&refresh=65825283b05f61703039619&ind=1612720414927&filename=Lucida%20Console%20Regular.zip";
    hash = "sha256-viy63jSEOhM6WmPSKz8lueZCNmv+V7anCwusliT6afc=";
  };
in
runCommand "lucida-console" { } ''
  mkdir -p $out/share/fonts
  cp "${font}/Lucida Console Regular.ttf" $out/share/fonts/
''
