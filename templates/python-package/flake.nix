{
  description = "A Nix-flake-based Python development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    inputs@{ self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      #       ↑ Swap it for your system if needed
      #       "aarch64-linux" / "x86_64-darwin" / "aarch64-darwin"
      pkgs = import nixpkgs { inherit system; };

      pyproject = builtins.fromTOML (builtins.readFile ./pyproject.toml);
      project = pyproject.project;

      package = pkgs.python3Packages.buildPythonPackage {
        pname = project.name;
        inherit (project) version;
        format = "pyproject";
        src = ./.;

        build-system = with pkgs.python3Packages; [
          setuptools
        ];

        # Test dependencies
        nativeCheckInputs = [
          pkgs.python3Packages.mypy
          pkgs.python3Packages.nox
          pkgs.python3Packages.pytest
          pkgs.python3Packages.ruff

          pkgs.taplo
        ];

        # Check phase: nix flake check
        checkPhase = ''
          runHook preCheck
          nox
          runHook postCheck
        '';

        # Production dependencies
        propagatedBuildInputs = [
          pkgs.python3Packages.click
        ];
      };

      editablePackage = pkgs.python3.pkgs.mkPythonEditablePackage {
        pname = project.name;
        inherit (project) scripts version;
        root = "$PWD";
      };

    in
    {
      # Project packages output: nix build
      packages.${system} = {
        "${project.name}" = package;
        default = self.packages.${system}.${project.name};
      };

      # Shell for development dependencies: nix develop
      devShells.${system}.default = pkgs.mkShell {
        name = "python-package-template";

        inputsFrom = [
          package
        ];

        packages = [
          # Our package
          editablePackage

          # Various tools
          pkgs.python3Packages.build
          pkgs.python3Packages.jupyter

          # Editor tools
          pkgs.basedpyright

          # Nix tools
          pkgs.nixfmt-rfc-style
          pkgs.nixd

          # If the dependencies need system libs, you usually need pkg-config + the lib
          # pkg-config
          # openssl
        ];
      };
    };
}
