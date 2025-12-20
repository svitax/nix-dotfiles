{ inputs, ... }:
let
  packagesDir = inputs.self + /packages;
  entries = builtins.readDir packagesDir;

  emacsPackagesDir = inputs.self + /packages/emacs-packages;
  emacsEntries = builtins.readDir emacsPackagesDir;
in
{
  # Custom packages from packages/ directory
  packages =
    final: prev:
    let
      packages = builtins.listToAttrs (
        builtins.attrValues (
          builtins.mapAttrs (name: type: {
            name = builtins.replaceStrings [ ".nix" ] [ "" ] name;
            value = final.callPackage (packagesDir + "/${name}") { };
          }) entries
        )
      );

      # Add custom packages from packages/emacs-packages to the epkgs set
      emacsPackagesFor =
        emacs:
        (prev.emacsPackagesFor emacs).overrideScope (
          efinal: eprev:
          builtins.listToAttrs (
            builtins.attrValues (
              builtins.mapAttrs (name: type: {
                name = builtins.replaceStrings [ ".nix" ] [ "" ] name;
                value = efinal.callPackage (emacsPackagesDir + "/${name}") {
                  inherit (final) lib pkgs;
                  epkgs = efinal;
                };
              }) emacsEntries
            )
          )
        );
    in
    packages // { inherit emacsPackagesFor; };

  # Patches to packages
  # patches = final: prev: {}

  # Unstable nixpkgs
  unstable-packages = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
      config.allowUnfree = true;
    };
  };
}
