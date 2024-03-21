path:
{ stdenv, lib, fetchurl, unzip }:

let version = "0.829";
in stdenv.mkDerivation {
  name = "pragmata-pro-${version}";
  src = fetchurl {
    url = "file://${path}/pragmatapro_${version}.zip";
    hash = "sha256-/DgsOMHi/bAE55SDgf5f59q81yvuVERSn/K5Y+D3Pyw=";
  };
  unpackPhase = "${lib.getExe unzip} $src";
  dontBuild = true;
  installPhase = ''
    mkdir -p $out/share/fonts/{truetype,opentype}

    cd PragmataPro${version}
    cp *.ttf $out/share/fonts/truetype/
  '';
}
