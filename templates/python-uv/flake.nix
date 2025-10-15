{
  description = "A Nix-flake-based Python development environment with uv";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    pyproject-nix = {
      url = "github:pyproject-nix/pyproject.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    uv2nix = {
      url = "github:pyproject-nix/uv2nix";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pyproject-build-systems = {
      url = "github:pyproject-nix/build-system-pkgs";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.uv2nix.follows = "uv2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      pyproject-nix,
      uv2nix,
      pyproject-build-systems,
      ...
    }:
    let
      system = "x86_64-linux";
      #       ↑ Swap it for your system if needed
      #       "aarch64-linux" / "x86_64-darwin" / "aarch64-darwin"
      pkgs = nixpkgs.legacyPackages.${system};
      name = "uv-template";

      workspace = uv2nix.lib.workspace.loadWorkspace { workspaceRoot = ./.; };

      overlay = workspace.mkPyprojectOverlay {
        sourcePreference = "wheel";
      };
      editableOverlay = workspace.mkEditablePyprojectOverlay {
        # Use environment variable pointing to editable root directory
        root = "$REPO_ROOT";
      };

      python = pkgs.python3;
      pythonBase = pkgs.callPackage pyproject-nix.build.packages {
        inherit python;
      };

      pythonSet = pythonBase.overrideScope (
        nixpkgs.lib.composeManyExtensions [
          pyproject-build-systems.overlays.wheel
          overlay
        ]
      );
      editablePythonSet = pythonSet.overrideScope editableOverlay;

      virtualenv = editablePythonSet.mkVirtualEnv "python-uv-dev" workspace.deps.all;
    in
    {
      # Project packages output: nix build
      packages.${system}.default = pythonSet.mkVirtualEnv "python-uv" workspace.deps.default;

      # Shell for development dependencies from pyproject.toml: nix develop
      devShells.${system}.default = pkgs.mkShell {
        name = name;
        packages = with pkgs; [
          virtualenv
          uv

          basedpyright
          black
          ruff

          taplo
          nixfmt-rfc-style
          nixd

          # If the dependencies need system libs, you usually need pkg-config + the lib
          # pkg-config
          # openssl
        ];
        env = {
          # Prevent uv from managing a virtual environment, this is managed by
          # uv2nix
          UV_NO_SYNC = "1";
          # Use interpreter path for all uv operations
          UV_PYTHON = pythonSet.python.interpreter;
          # Prevent uv from downloading managed Python interpreters, we use Nix
          # instead
          UV_PYTHON_DOWNLOADS = "never";
        };
        # Unset PYTHONPATH to eliminate bad side effects from Nixpkgs Python
        # builders
        # Set REPO_ROOT to inform the virtualenv which directory editable
        # packages are relative to
        shellHook = ''
          unset PYTHONPATH
          export REPO_ROOT=$(git rev-parse --show-toplevel)
        '';
      };
    };
}
