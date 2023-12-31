{
  description = "A Nix-flake-based Python development environment with Poetry";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      formatter.${system} = pkgs.alejandra;
      devShells.${system}.default = pkgs.mkShell {
        name = "nix-poetry-template";
        packages = [
          pkgs.python3
          pkgs.poetry
          pkgs.ruff-lsp
          pkgs.nodePackages.pyright
          pkgs.python311Packages.debugpy
          # pkgs.mypy # needs to be installed in the venv through poetry for neovim to pick up
          pkgs.nil
          pkgs.alejandra
          pkgs.taplo
        ];
        # Workaround in linux: python downloads ELF's that can't find glibc
        # You would see errors like: error while loading shared libraries: name.so: cannot open shared object file: No such file or directory
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
          pkgs.stdenv.cc.cc
          pkgs.zlib
          # Add any missing library needed
          # You can use the nix-index package to locate them, e.g. nix-locate -w --top-level --at-root /lib/libudev.so.1
        ];
        # Put the venv in the repo, so direnv can access it
        POETRY_VIRTUALENVS_IN_PROJECT = "true";
        POETRY_VIRTUALENVS_PATH = "{project-dir}/.venv";
        # Use python from path, so you can use a different version to the one bundled with poetry
        POETRY_VIRTUALENVS_PREFER_ACTIVE_PYTHON = "true";
      };
    };
}
