switch:
	nixos-rebuild switch --flake ".#somewhere"
update:
	nix flake update --recreate-lock-file --commit-lock-file
