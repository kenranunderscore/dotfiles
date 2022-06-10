{ lib, stdenv, fetchFromGitHub, autoreconfHook, ncurses5, SDL2, SDL2_image
, SDL2_sound, SDL2_mixer, SDL2_ttf,
# Override this to enable the SDL2 frontend for Angband, for instance:
# pkgs.angband.override { enableSdl2 = true; }.
enableSdl2 ? false }:

stdenv.mkDerivation rec {
  pname = "angband";
  version = "4.2.4";

  src = fetchFromGitHub {
    owner = "angband";
    repo = "angband";
    rev = version;
    sha256 = "sha256-Fp3BGCZYYdQCKXOLYsT4zzlibNRlbELZi26ofrbGGPQ=";
  };

  configureFlags = lib.optionals enableSdl2 [ "--enable-sdl2" ];
  nativeBuildInputs = [ autoreconfHook ];
  buildInputs = [ ncurses5 ] ++ lib.optionals enableSdl2 [
    SDL2
    SDL2_image
    SDL2_sound
    SDL2_mixer
    SDL2_ttf
  ];
  installFlags = [ "bindir=$(out)/bin" ];

  meta = with lib; {
    homepage = "https://angband.github.io/angband";
    description = "A single-player roguelike dungeon exploration game";
    maintainers = [ maintainers.chattered ];
    license = licenses.gpl2;
  };
}
