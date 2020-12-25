install:
	nixos-rebuild switch --flake ".#somewhere"
update:
	nix flake update
