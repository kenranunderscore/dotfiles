NIX_CFG_HOME = ~/.config/nixpkgs

# We assume that a Nix channel named <nixpkgs> already exists. Then we look for
# the file ./hosts/<HOST>.nix (where HOST should be set from within the shell)
# and make this the config file for home-manager on this machine.
bootstrap:
	nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
	nix-channel --update
	nix-shell '<home-manager>' -A install
	mkdir -p $(NIX_CFG_HOME)
	ln -sf $(PWD)/hosts/$(HOST).nix $(NIX_CFG_HOME)/home.nix
	home-manager switch
