
somewhere:
	nixos-rebuild switch --flake ".#somewhere"
# For now it's impure, but soon it shall become P U R E
home-manager:
	nix build --impure ".#home" -o "./home-manager/result"
	nix shell nixpkgs/nixos-20.09#nix -c "./home-manager/result/activate"
update:
	nix flake update
