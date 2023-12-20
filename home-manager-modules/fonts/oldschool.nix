{ runCommand, fetchurl, unzip }:

# This is an ABSOLUTELY AWESOME collection of (afaict hand-made
# remakes of) classic PC fonts, made by VileR.  Check it out at
# https://int10h.org/oldschool-pc-fonts
let
  font = fetchurl {
    url =
      "https://int10h.org/oldschool-pc-fonts/download/oldschool_pc_font_pack_v2.2_linux.zip";
    hash = "sha256-sw3D7MmTGtLdi+dRfdAYE8iDShkRtYKrdkMZG0Gj11k=";
  };
in runCommand "install_oldschool_fonts" { } ''
  dest=$out/share/fonts/oldschool
  mkdir -p $dest
  ${unzip}/bin/unzip ${font} -d font
  cp -R font/"otb - Bm (linux bitmap)"/* $dest/
  cp -R font/"ttf - Ac (aspect-corrected)"/* $dest/
  cp -R font/"ttf - Mx (mixed outline+bitmap)"/* $dest/
  cp -R font/"ttf - Px (pixel outline)"/* $dest/
''
