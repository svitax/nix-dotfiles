{
  description = "A Nix-flake-based development environment with Jupyter";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = { self, nixpkgs, flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      perSystem = { pkgs, system, ... }:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          devShells.default = pkgs.mkShell {
            name = "jupyter-demo";
            packages = [ pkgs.python3 pkgs.poetry pkgs.pyright pkgs.ruff-lsp ];
            env = {
              # Workaround in linux: python downloads ELF's that can't find glibc
              # You would see errors like: error while loading shared libraries: name.so cannot open shared object file: No such file or directory
              LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
                pkgs.stdenv.cc.cc
                # Add any missing library needed
                # You can use the nix-index package to locate them, e.g. nix-locate -w --top-level --at-root /lib/libudev.so.1
              ];
              # Put the venv in the repo, so direnv can access it
              POETRY_VIRTUALENVS_IN_PROJECT = "true";
              POETRY_VIRTUALENVS_PATH = "{project-dir}/.venv";
              # Use python from path, so you can use a different version to the one bundled with poetry
              POETRY_VIRTUALENVS_PREFER_ACTIVE_PYTHON = "true";
            };
            #
          };
          # Formatter for your nix files, available through 'nix fmt'
          # Other options beside 'alejandra' include 'nixpkgs-fmt'
          formatter = pkgs.alejandra;
        };
      flake = { };
    };
}
