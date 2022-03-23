{ pkgs }:

# Copied from input-fonts/default.nix in nixpkgs, since in order to
# customize the font itself one needs to use a specific URL.  But
# that's not customizable in the derivation in nixpkgs.

let
  input = pkgs.input-fonts.override { acceptLicense = true; };
  lib = pkgs.lib;
in input.overrideAttrs (_: {
  src = pkgs.fetchzip {
    name = "input-fonts-${pkgs.input-fonts.version}";
    # Add .zip parameter so that zip unpackCmd can match it.
    url =
      "https://input.djr.com/build/?fontSelection=whole&a=0&g=0&i=topserif&l=serifs_round&zero=0&asterisk=height&braces=0&preset=default&line-height=1.2&accept=I+do&email=&.zip";
    sha256 = "sha256-GacoUKRJAG+4NEXFb2u3eVFZ7L6rRlDMhLQDY4maHWw=";
    stripRoot = false;

    extraPostFetch = ''
      # Reset the timestamp to release date for determinism.
      PATH=${lib.makeBinPath [ pkgs.python3.pkgs.fonttools ]}:$PATH
      for ttf_file in $out/Input_Fonts/*/*/*.ttf; do
        ttx_file=$(dirname "$ttf_file")/$(basename "$ttf_file" .ttf).ttx
        ttx "$ttf_file"
        rm "$ttf_file"
        touch -m -t ${
          builtins.replaceStrings [ "-" ] [ "" ] "2015-06-24"
        }0000 "$ttx_file"
        ttx --recalc-timestamp "$ttx_file"
        rm "$ttx_file"
      done
    '';
  };
})
