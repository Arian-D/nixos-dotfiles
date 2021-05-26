update:
	nix flake update --commit-lock-file

switch-host:
	nixos-rebuild switch --flake ".#somewhere"

build-home-manager:
	export NIXPKGS_ALLOW_UNFREE=1
	nix build ".#home" -o "./home-manager/result"

activate-home-manager: build-home-manager
	./home-manager/result/activate
