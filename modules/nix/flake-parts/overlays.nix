{
  inputs,
  ...
}:
let
  packagesDir = inputs.self + /packages;
  entries = builtins.readDir packagesDir;

  emacsPackagesDir = inputs.self + /packages/emacs-packages;
  emacsEntries = builtins.readDir emacsPackagesDir;
in
{
  flake.overlays.packages =
    final: prev:
    let
      packages = builtins.listToAttrs (
        builtins.attrValues (
          builtins.mapAttrs (name: type: {
            name = builtins.replaceStrings [ ".nix" ] [ "" ] name;
            value = final.callPackage (packagesDir + "/${name}") {};
          }) entries
        )
      );
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
}
