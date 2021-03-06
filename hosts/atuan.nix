{ config, lib, pkgs, ... }:

{
  imports = [ ./base.nix ../modules ];
  hosts.base.username = "kenran";
}
