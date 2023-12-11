{
  description = "Neovim derivation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Add bleeding-edge plugins here.
    # They can be updated with `nix flake update` (make sure to commit the generated flake.lock)
    # wf-nvim = {
    #   url = "github:Cassin01/wf.nvim";
    #   flake = false;
    # };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }: let
    inherit (self) outputs;
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

      perSystem = {
        pkgs,
        system,
        ...
      }: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            neovim-overlay
          ];
        };
        shell = pkgs.mkShell {
          name = "nvim";
          buildInputs = with pkgs; [
            lua-language-server
            nil
            stylua
            luajitPackages.luacheck
          ];
        };
      in {
        packages = rec {
          default = nvim;
          nvim = pkgs.nvim-fennec;
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
