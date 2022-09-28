{ runCommand, fetchurl }:

let
  font = fetchurl {
    url = "https://www.dafontfree.net/data/11/l/60383/Lucida%20Console.ttf";
    sha256 = "sha256-bd9k7oltJM+ZCPEVriIKfPoY3ANLxKaOTbaNzVfHFRI=";
  };
in runCommand "lucida-console" { } ''
  mkdir -p $out/share/fonts
  cp ${font} $out/share/fonts/Lucida\ Console.ttf
''
