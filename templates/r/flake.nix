{
  description = "A Nix-flake-based R development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      ...
    }:
    let
      system = "x86_64-linux";
      #       ↑ Swap it for your system if needed
      #       "aarch64-linux" / "x86_64-darwin" / "aarch64-darwin"
      pkgs = import nixpkgs { inherit system; };
      name = "r-template";
    in
    {
      # Project packages output: nix build
      # packages.${system}.default = pkgs.rPackages.buildRPackage{};

      # Shell for development dependencies from pyproject.toml: nix develop
      devShells.${system}.default = pkgs.mkShell {
        name = name;

        packages = with pkgs; [
          R

          nixfmt-rfc-style
          nixd

          # If the dependencies need system libs, you usually need pkg-config + the lib
          # pkg-config
          # openssl
        ];
      };
    };
}
