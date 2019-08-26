home-manager:
	mkdir -p home-manager 
	cp -r ~/.config/nixpkgs/* home-manager
system:
	mkdir -p nixos
	cp -r /etc/nixos/* nixos
	$(RM) -f nixos/hardware-configuration.nix

clean-system:
	$(RM) -rf nixos
clean-home-manager:
	$(RM) -rf home-manager 
