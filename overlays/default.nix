{ inputs, ... }:
let
  pkgsDir = inputs.self + /pkgs;
  entries = builtins.readDir pkgsDir;

  emacsPkgsDir = inputs.self + /pkgs/emacs-packages;
  emacsEntries = builtins.readDir emacsPkgsDir;
in
{
  # Custom packages from pkgs/ directory
  packages =
    final: prev:
    let
      packages = builtins.listToAttrs (
        builtins.attrValues (
          builtins.mapAttrs (name: type: {
            name = builtins.replaceStrings [ ".nix" ] [ "" ] name;
            value = final.callPackage (pkgsDir + "/${name}") { };
          }) entries
        )
      );

      # Add custom packages from pkgs/emacs-packages to the epkgs set
      emacsPackagesFor =
        emacs:
        (prev.emacsPackagesFor emacs).overrideScope (
          efinal: eprev:
          builtins.listToAttrs (
            builtins.attrValues (
              builtins.mapAttrs (name: type: {
                name = builtins.replaceStrings [ ".nix" ] [ "" ] name;
                value = efinal.callPackage (emacsPkgsDir + "/${name}") {
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
