{
  description = "My beautiful flakey Nix config";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-20.09";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-20.09";
    };
  };

  outputs = inputs @ { nixpkgs, nixpkgs-unstable, home-manager, ... }: {
    nixosConfigurations.somewhere = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./nixos/configuration.nix
        nixpkgs.nixosModules.notDetected
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.someone = import ./home-manager/home.nix;
          home-manager.verbose = true;
        }
      ];
    };
  };
}
