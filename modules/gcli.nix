{ config, pkgs, lib, ... }:

let
  gcli = pkgs.callPackage ({ lib, fetchFromGitHub, stdenv, curl, autoreconfHook
    , pkg-config, yacc, flex }:

    stdenv.mkDerivation rec {
      pname = "gcli";
      version = "2.0.0";
      src = fetchFromGitHub {
        owner = "herrhotzenplotz";
        repo = "gcli";
        rev = version;
        hash = "sha256-ry+T39gFVPfHazAbv97UFpMIH1Dbbw6tZwsn9V4uRec=";
      };
      nativeBuildInputs = [ autoreconfHook pkg-config yacc flex ];
      buildInputs = [ curl ];
    }) { };
  cfg = config.modules.gcli;
in {
  options.modules.gcli.enable = lib.mkEnableOption "gcli";
  config = lib.mkIf cfg.enable { home.packages = [ gcli ]; };
}
