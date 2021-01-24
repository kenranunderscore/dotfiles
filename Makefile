NIX_CFG_HOME = ~/.config/nixpkgs

# We assume that a Nix channel named <nixpkgs> already exists
bootstrap:
	nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
	nix-channel --update
	nix-shell '<home-manager>' -A install
	mkdir -p $(NIX_CFG_HOME)
	ln -sf $(PWD)/home.nix $(NIX_CFG_HOME)/home.nix
	home-manager switch
