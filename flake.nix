{
  description = "My beautiful flakey Nix config";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-20.09";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    home-manager = {
      url = "github:nix-community/home-manager/release-20.09";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nyxt = {
      url = "github:atlas-engineer/nyxt/2-pre-release-5";
      flake = true;
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs = inputs @ { self
                     , nixpkgs
                     , nixpkgs-unstable
                     , nixpkgs-master
                     , home-manager
                     , nyxt
                     , emacs-overlay
                     , ... }:
    {
      # NixOS (Somewhere)
      nixosConfigurations.somewhere = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./nixos/configuration.nix
          nixpkgs.nixosModules.notDetected
        ];
      };

      # Home-manager
      home = (home-manager.lib.homeManagerConfiguration {
        configuration =
          { pkgs, ... }:
          {
            nixpkgs.overlays = [
              emacs-overlay.outputs.overlay
            ];
            imports = [./home-manager/home.nix];
            home.packages = with nixpkgs-master.legacyPackages.x86_64-linux; [
              nyxt discord
            ];
          };
        system = "x86_64-linux";
        homeDirectory = "/home/someone";
        username = "someone";
      }).activationPackage;
    };
}
