# This file defines overlays
{ inputs, ... }:
let
  addPatches = pkg: patches:
    pkg.overrideAttrs
    (oldAttrs: { patches = (oldAttrs.patches or [ ]) ++ patches; });
in {
  # This one brings our custom packages from the 'pkgs' directory
  additions = final: prev: import ../pkgs { pkgs = final; };

  # This one contains whatever you want to overlay
  # You can change versions, add patches, set compilation flags, anything really
  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    # example = prev.example.overrideAttrs (oldAttrs: rec {
    # ...
    # });
    # my papis patch to add --link flag to bibtex import
    # papis bibtex read ~/OneDrive/docs/lib.bib import --all --link
    papis = addPatches prev.papis [ ./papis-bibtex-import-link.patch ];
    tmux-sessionizer = addPatches prev.tmux-sessionizer [
      ./tmux-sessionizer-reverse-layout.patch
      ./tmux-sessionizer-switch-attatch-if-outside-tmux.patch
    ];
  };

  # zjstatus zellij statusbar plugin
  zjstatus = final: prev: {
    zjstatus = inputs.zjstatus.packages.${prev.system}.default;
  };
}
