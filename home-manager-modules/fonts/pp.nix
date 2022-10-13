path:
{ lib, fetchzip }:

let version = "0.829";
in fetchzip {
  name = "pragmata-pro-${version}";

  url = "file://${path}/pragmatapro_${version}.zip";

  sha256 = "sha256-IFrS/u68vf2xmjWSjQCFAz7uTuT6lUj7zJYajSJWEtw=";

  postFetch = ''
    mkdir -p $out/share/fonts/
    unzip -j $downloadedFile \*.otf -d $out/share/fonts/opentype
    unzip -j $downloadedFile \*.ttf -d $out/share/fonts/truetype
  '';
}
