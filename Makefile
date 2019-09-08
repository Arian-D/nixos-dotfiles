all: home-manager system emacs stump
home-manager:
	mkdir -p .config/
	cp -r ~/.config/nixpkgs/ .config/
system:
	mkdir -p nixos
	cp -r /etc/nixos/ .
emacs:
	cp -r ~/.emacs.d/ .
stump:
	cp -rf ~/.stumpwm.d/ .
