{
  description = "My config";
  inputs = {
    nixpkgs.url = "nixpkgs/release-20.09";
    # nixpkgs-unstable.url = "nixpkgs/master";
  };

  outputs = { self, nixpkgs }: {
    nixosConfigurations.somewhere = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ./nixos/configuration.nix ];
    };
  };
}
