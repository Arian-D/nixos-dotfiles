{
  description = "My beautiful flakey Nix config";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-20.09";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-20.09";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    nyxt = {
      url = "github:atlas-engineer/nyxt/2-pre-release-5";
      flake = true;
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, home-manager, nyxt, ... }:
    {
      nixosConfigurations.somewhere = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./nixos/configuration.nix
          nixpkgs.nixosModules.notDetected
        ];
      };

      homeConfigurations.home =
        home-manager.lib.homeManagerConfiguration {
          configuration = { pkgs, ... }:
            {
              imports = [./home-manager/home.nix];
              home.packages = [ nyxt.defaultPackage.x86_64-linux ];
            };
          system = "x86_64-linux";
          homeDirectory = "/home/someone";
          username = "someone";
        };
      home = self.homeConfigurations.home.activationPackage;
    };
}
