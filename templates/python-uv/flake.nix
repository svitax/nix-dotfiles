{
  description = "A Nix-flake-based Python development environment with uv";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    inputs@{ self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      #       ↑ Swap it for your system if needed
      #       "aarch64-linux" / "x86_64-darwin" / "aarch64-darwin"
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      # Shell for dev dependencies
      #
      #     nix develop
      #
      # Use this shell for developing your app
      devShells.${system}.default = pkgs.mkShell {
        name = "nix-python-template";
        packages = with pkgs; [
          uv

          nixfmt-rfc-style
          nixd

          # These can be provided by uv.
          jupyter
          basedpyright
          black
          ruff

          # If the dependencies need system libs, you usually need pkg-config + the lib
          # pkg-config
          # openssl
        ];

      };
    };
}
