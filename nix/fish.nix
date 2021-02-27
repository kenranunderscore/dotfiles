{ pkgs, ... }:

{
  programs.fish = {
    enable = true;
    plugins = [
      {
        name = "spacefish";
        src = pkgs.fetchFromGitHub {
          owner = "matchai";
          repo = "spacefish";
          rev = "b1023a9d60fe1ae7234721ad2569b7c563aac46a";
          sha256 = "0abj7g9kydb1am8kwx136wzlvj90z9ic2lfr2gg20jfr5k0s32f9";
        };
      }
      {
        name = "fish-async-prompt";
        src = pkgs.fetchFromGitHub {
          owner = "acomagu";
          repo = "fish-async-prompt";
          rev = "40f30a4048b81f03fa871942dcb1671ea0fe7a53";
          sha256 = "19i59145lsjmidqlgk2dmvs3vg2m3zlz2rcms2kyyk1m3y63q8xi";
        };
      }
      {
        name = "autopair.fish";
        src = pkgs.fetchFromGitHub {
          owner = "jorgebucaran";
          repo = "autopair.fish";
          rev = "1222311994a0730e53d8e922a759eeda815fcb62";
          sha256 = "0lxfy17r087q1lhaz5rivnklb74ky448llniagkz8fy393d8k9cp";
        };
      }
    ];
    shellAbbrs = import ./shell-aliases.nix;
  };
}
