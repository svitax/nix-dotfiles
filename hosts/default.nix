{ inputs, ... }:
let
  inherit (inputs.nixpkgs.lib) nixosSystem;
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  inherit (inputs.self) outputs;
in
{
  flake = {
    nixosConfigurations = {
      erasmus = nixosSystem {
        system = "x86_64-linux";
        specialArgs = {
          inherit inputs outputs;
        };
        modules = [ ./erasmus/configuration.nix ];
      };
    };
    homeConfigurations = {
      "evermind@erasmus" = homeManagerConfiguration {
        pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
        extraSpecialArgs = {
          inherit inputs outputs;
        };
        modules = [ ./erasmus/home.nix ];
      };
    };
  };
}
