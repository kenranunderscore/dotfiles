{ runCommand, fetchFromGitHub }:

let
  font = fetchFromGitHub {
    owner = "neutaaaaan";
    repo = "termingus";
    sha256 = "sha256-yHXKZF5+eRgzzuhSCx5X0d7Pci6RGguTLFg04o/1OkI=";
    rev = "f73425d339b5c85205d01422260687e0d991c1bb";
  };
in runCommand "install_termingus" { } ''
  mkdir -p $out/share/fonts
  cp ${font}/*.bdf $out/share/fonts/
''
