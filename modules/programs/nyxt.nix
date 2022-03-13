{ config, lib, pkgs, ... }:

let
  cfg = config.modules.programs.nyxt;

  # The currently packaged version of Nyxt is broken due to some
  # FHS-related issue.  So for now I download the current release
  # version and put in in the nix store manually.
  nyxtBin = pkgs.stdenv.mkDerivation {
    name = "nyxt-bin";
    src = builtins.fetchurl {
      url =
        "https://github.com/atlas-engineer/nyxt/releases/download/2.2.4/nyxt-2.2.4.tar.xz";
      sha256 = "1m47hgqdk1rs4dl9pndiiiqx4v3n5r5m8m3n8xqslzp388hab1fg";
    };
    unpackPhase = "tar xf $src";
    installPhase = ''
      mkdir -p $out/bin
      cp -r * $out
      ln -sf $out/usr/local/bin/nyxt $out/bin/nyxt
    '';
    dontFixup = true;
  };
in {
  options.modules.programs.nyxt.enable = lib.mkEnableOption "nyxt";

  config = lib.mkIf cfg.enable { home.packages = [ nyxtBin ]; };
}
