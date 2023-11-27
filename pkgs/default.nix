{pkgs ? import <nixpkgs> {}}: rec {
  # Custom packages that can be defined similarly to ones from nixpkgs
  # You can build them using 'nix build .#example'
  # example = pkgs.callPackage ./example { };

  firefox-gnome-theme = pkgs.callPackage ./firefox-gnome-theme.nix {};

  # Personal scripts
  live-grep = pkgs.callPackage ./live-grep {};
  helix-live-grep = pkgs.callPackage ./helix-live-grep {};
  zellij-smart-sessionizer = pkgs.callPackage ./zellij-smart-sessionizer {};
}
