#!/usr/bin/env fish

if test -n "$HOME" ;
  set -xg NIX_LINK "$HOME/.nix-profile"

  # Set the default profile
  if not test -L "$NIX_LINK" ;
    echo "creating $NIX_LINK" >&2
    set -l _NIX_DEF_LINK /nix/var/nix/profiles/default
    /nix/store/hr4y6cdbg56hc8b6vjdim5p7247gkk8z-coreutils-8.30/bin/ln -s "$_NIX_DEF_LINK" "$NIX_LINK"
  end

  set -xg PATH $NIX_LINK/bin $PATH

  # Subscribe to nixpkgs channel
  if not test -e $HOME/.nix-channels ;
    echo "https://nixos.org/channels/nixpkgs-unstable nixpkgs" > $HOME/.nix-channels
  end

  set -xg NIX_PATH $NIX_PATH $HOME/.nix-defexpr/channels/nixpkgs

  if test -e /etc/ssl/certs/ca-bundle.crt ;
    set -xg SSL_CERT_FILE /etc/ssl/certs/ca-bundle.crt
  else if test -e /etc/ssl/certs/ca-certificates.crt ;
    set -xg SSL_CERT_FILE /etc/ssl/certs/ca-certificates.crt
  else if test -e "$NIX_LINK/etc/ca-bundle.crt" ;
    set -xg SSL_CERT_FILE "$NIX_LINK/etc/ca-bundle.crt"
  end
end
