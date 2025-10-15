{
  description = "A basic flake using pyproject.toml project metadata";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    pyproject-nix.url = "github:pyproject-nix/pyproject.nix";
    pyproject-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      pyproject-nix,
      ...
    }:
    let
      system = "x86_64-linux";
      #       ↑ Swap it for your system if needed
      #       "aarch64-linux" / "x86_64-darwin" / "aarch64-darwin"
      pkgs = import nixpkgs { inherit system; };
      name = "pyproject-template";
      # Loads pyproject.toml into a high-level project representation
      # Do you notice how this is not tied to any 'system' attribute or package
      # sets? That is because 'project' refers to a pure data representation.
      project = pyproject-nix.lib.project.loadPyproject {
        # Read & unmarshal pyproject.toml relative to this project root.
        # projectRoot is also used to set 'src' for renderers such as
        # buildPythonPackage.
        projectRoot = ./.;
      };

      # We are using the default nixpkgs Python3 interpreter & packages set.
      #
      # This means that we are purposefully ignoring:
      # - Version bounds
      # - Dependency sources (meaning local path dependencies won't resolve to
      #   the local path)
      #
      # To use packages from local sources see "Overriding Python packages" in
      # the nixpkgs manual:
      # https://nixos.org/manual/nixpkgs/stable/#reference
      #
      # Or use an overlay generator such as uv2nix:
      # https://github.com/pyproject-nix/uv2nix
      python = pkgs.python3;

      # Returns a function that can be passed to 'python.withPackages'
      arg = project.renderers.withPackages { inherit python; };
      # Returns an attribute set that can be passed to 'buildPythonPackage'
      attrs = project.renderers.buildPythonPackage { inherit python; };
    in
    {
      # Project packages output: nix build
      packages.${system}.default = (python.pkgs.buildPythonPackage attrs);

      # Shell for development dependencies from pyproject.toml: nix develop
      devShells.${system}.default = pkgs.mkShell {
        name = name;

        packages = with pkgs; [
          (python.withPackages arg)

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
      };
    };
}
