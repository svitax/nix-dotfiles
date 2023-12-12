{
  description = "Neovim derivation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    neovim-src = {
      url = "github:neovim/neovim";
      flake = false;
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }: let
    # This is where the Neovim derivation is built.
    neovim-overlay = import ./nix/neovim-overlay.nix {inherit inputs;};
  in
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem = {system, ...}: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            neovim-overlay
          ];
        };
        shell = pkgs.mkShell {
          name = "nvim";
          buildInputs = with pkgs; [
            npins
            lua-language-server
            nil
            stylua
            luajitPackages.luacheck
          ];
        };
      in {
        packages = rec {
          default = nvim;
          nvim = pkgs.nvim-pkg;
        };
        devShells = {
          default = shell;
        };
      };

      flake = {
        # You can add this overlay to your NixOS configuration
        overlays.default = neovim-overlay;
      };
    };
}
