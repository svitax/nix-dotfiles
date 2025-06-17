{
  # This template turns a Python script into an executable program as a Nix
  # Flake output.
  description = "A Nix-flake-based Python development environment for single file scripts.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    inputs@{ self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      #       ↑ Swap it for your system if needed
      #       "aarch64-linux" / "x86_64-darwin" / "aarch64-darwin"
      pkgs = import nixpkgs { inherit system; };
      name = "python-script-template";
      script = builtins.readFile ./script.py;

      program = pkgs.writers.writePython3Bin name {
        libraries = [ pkgs.python3Packages.click ];
      } script;
    in
    {
      # Project packages output: nix build
      packages = {
        "${name}" = program;
        default = self.packages.${system}.${name};
      };

      # Shell for development dependencies: nix develop
      devShells.${system}.default = pkgs.mkShell {
        name = "${name}";

        inputsFrom = [
          program
        ];

        packages = with pkgs; [
          nixfmt-rfc-style
          nixd

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
