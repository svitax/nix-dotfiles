{
  description = "A Nix-flake-based Python development environment";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = {nixpkgs, ...} @ inputs: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    formatter.${system} = pkgs.alejandra;
    devShells.${system}.default = pkgs.mkShell {
      name = "nix-python-template";
      packages = [
        (pkgs.python3.withPackages (python-pkgs: [
          python-pkgs.numpy
          python-pkgs.pandas
          python-pkgs.scipy
          python-pkgs.matplotlib
          python-pkgs.requests
        ]))
        pkgs.ruff-lsp
        pkgs.nodePackages.pyright
        pkgs.nixd
        pkgs.alejandra
      ];
      # Workaround: make vscode's python extension read the .venv
      shellHook = ''
        venv="$(cd $(dirname $(which python)); cd ..; pwd)"
        ln -Tsf "$venv" .venv
      '';
    };
  };
}
