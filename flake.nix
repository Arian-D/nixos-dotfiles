{
  description = "My beautiful flakey Nix config";
  inputs = {
    nixpkgs.url = "nixpkgs/release-20.09";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-20.09";
    };
  };

  outputs = { nixpkgs, home-manager, ... }: {
    nixosConfigurations.somewhere = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./nixos/configuration.nix
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.someone = import ./nixpkgs/home.nix;
        }
      ];
    };
  };
}
