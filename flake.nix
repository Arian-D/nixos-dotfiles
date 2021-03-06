{
  description = "My beautiful flakey Nix config";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    home-manager = {
      url = "github:nix-community/home-manager";
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
      homeConfigurations.home =
        home-manager.lib.homeManagerConfiguration {
          configuration =
            { pkgs, ... }:
            {
              nixpkgs.config.allowUnfree = true;
              nixpkgs.overlays = [
                emacs-overlay.outputs.overlay
              ];
              imports = [./home-manager/home.nix];
            };
          system = "x86_64-linux";
          homeDirectory = "/home/someone";
          username = "someone";
        };
      home = self.homeConfigurations.home.activationPackage;

      devShell.x86_64-linux =
        nixpkgs.legacyPackages.x86_64-linux.callPackage
          ./shell.nix {};
    };

}
