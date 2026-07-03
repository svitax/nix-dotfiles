{
  self,
  inputs,
  lib,
  ...
}:
let
  autowire = import ./lib/autowire.nix { inherit lib; };
  root = ./.;
in
{
  perSystem =
    { system, pkgs, ... }:
    {
      packages = autowire.discoverPackages {
        dir = root + /packages;
        inherit pkgs;
      };

      # TODO: set formatter to treefmt-nix
      formatter = pkgs.nixfmt-rfc-style;
    };

  flake = {
    overlays = import ./overlays { inherit inputs; };

    nixosModules = autowire.discoverModules { dir = root + /modules/nixos; };
    darwinModules = autowire.discoverModules { dir = root + /modules/darwin; };
    homeManagerModules = autowire.discoverModules { dir = root + /modules/home; };

    nixosConfigurations = autowire.discoverNixosConfigurations {
      dir = root + /hosts;
      inherit inputs;
      outputs = self;
      nixosModules = self.nixosModules;
    };
    darwinConfigurations = autowire.discoverDarwinConfigurations {
      dir = root + /hosts;
      inherit inputs;
      outputs = self;
      darwinModules = self.darwinModules;
    };
    homeConfigurations = autowire.discoverHomeConfigurations {
      dir = root + /hosts;
      inherit inputs;
      outputs = self;
      homeModules = self.homeManagerModules;
    };
  };
}
