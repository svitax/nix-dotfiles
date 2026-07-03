# Autowiring helper functions for auto-discovering configurations and modules
{ lib }:
let
  inherit (lib)
    filterAttrs
    mapAttrs'
    nameValuePair
    hasSuffix
    removeSuffix
    attrNames
    pathExists
    concatMapAttrs
    elem
    ;
  inherit (builtins) readDir;

  # Helper: Filter and map attributes in one pass
  mapFilterAttrs =
    pred: f: attrs:
    filterAttrs pred (mapAttrs' f attrs);

  # Helper: Check if path is a valid nix file or directory with default.nix
  isNixEntry =
    name: type:
    (type == "regular" && hasSuffix ".nix" name && name != "default.nix")
    || (type == "directory" && pathExists (name + "/default.nix"));
in
{
  # Discover and import modules from a directory
  # Each .nix file or directory becomes a module export
  discoverModules =
    { dir }:
    mapFilterAttrs (_: v: v != null) (
      name: type:
      let
        isValidNixFile = type == "regular" && hasSuffix ".nix" name && name != "default.nix";
        isValidDir = type == "directory" && pathExists (dir + "/${name}/default.nix");
      in
      if isValidNixFile then
        nameValuePair (removeSuffix ".nix" name) (import (dir + "/${name}"))
      else if isValidDir then
        nameValuePair name (import (dir + "/${name}"))
      else
        nameValuePair "" null
    ) (safeReadDir dir);

  # Discover packages from a directory
  # Each .nix file should be callPackage-compatible
  discoverPackages =
    { dir, pkgs }:
    mapFilterAttrs (_: v: v != null) (
      name: type:
      if type == "regular" && hasSuffix ".nix" name && name != "default.nix" then
        nameValuePair (removeSuffix ".nix" name) (pkgs.callPackage (dir + "/${name}") { })
      else
        nameValuePair "" null
    ) (safeReadDir dir);

  # Discover NixOS configurations
  # hosts/<host>/nixos.nix -> nixosConfigurations.<host>
  # NOTE: Each host config should set nixpkgs.hostPlatform to define its architecture
  discoverNixosConfigurations =
    {
      dir,
      inputs,
      outputs,
      nixosModules,
      specialArgs ? { },
    }:
    mapFilterAttrs (_: v: v != null) (
      name: type:
      let
        hostDir = dir + "/${name}";
        hasConfig = pathExists (hostDir + "/nixos.nix");
      in
      if type == "directory" && hasConfig then
        nameValuePair name (
          inputs.nixpkgs.lib.nixosSystem {
            # System is determined by nixpkgs.hostPlatform in each host's config
            modules = [
              (hostDir + "/nixos.nix")
              # Import all discovered NixOS modules
              { imports = builtins.attrValues nixosModules; }
            ];
            specialArgs = specialArgs // {
              inherit inputs outputs;
            };
          }
        )
      else
        nameValuePair "" null
    ) (safeReadDir dir);

  # Discover Darwin configurations
  # hosts/<host>/darwin.nix -> darwinConfigurations.<host>
  discoverDarwinConfigurations =
    {
      dir,
      inputs,
      outputs,
      darwinModules,
      specialArgs ? { },
    }:
    let
      nixpkgs-unstable = inputs.nixpkgs-unstable;
    in
    mapFilterAttrs (_: v: v != null) (
      name: type:
      let
        hostDir = dir + "/${name}";
        hasDarwin = pathExists (hostDir + "/darwin.nix");
      in
      if type == "directory" && hasDarwin then
        nameValuePair name (
          inputs.nix-darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            modules = [
              (hostDir + "/darwin.nix")
              # Import all discovered Darwin modules
              { imports = builtins.attrValues darwinModules; }
            ];
            specialArgs = specialArgs // {
              inherit inputs outputs;
              pkgs-unstable = import nixpkgs-unstable {
                system = "aarch64-darwin";
                config.allowUnfree = true;
              };
            };
          }
        )
      else
        nameValuePair "" null
    ) (safeReadDir dir);

  # Discover Home Manager configurations
  # hosts/<host>/users/<user>.nix -> homeConfigurations."user@host"
  discoverHomeConfigurations =
    {
      dir,
      inputs,
      outputs,
      homeModules,
      extraSpecialArgs ? { },
    }:
    let
      nixpkgs = inputs.nixpkgs;
      getSystem = hostToSystem { inherit darwinDir; };

      hostDirs = filterAttrs (_: type: type == "directory") (safeReadDir dir);
    in
    mapFilterAttrs (_: v: v != null) (
      name: type:
      let
        hostDir = dir + "/${name}";
        hasHome = pathExists (hostDir + "/home.nix");
      in
      if type == "directory" && hasHome then
        let
          system = getSystem name;
        in
        # TODO: the hardcoded evermind is what is bothering me here
        # what happens when i do
        nameValuePair "evermind@${name}" (
          inputs.home-manager.lib.homeManagerConfiguration {
            pkgs = inputs.nixpkgs.legacyPackages.${system};
            modules = [
              (hostDir + "/home.nix")
              { imports = builtins.attrValues homeModules; }
            ];
            extraSpecialArgs = extraSpecialArgs // {
              inherit inputs outputs;
            };
          }
        )
      else
        nameValuePair "" null
    ) (safeReadDir dir);

}
