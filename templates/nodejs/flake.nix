{
  description = "A Nix-flake-based NodeJS development environment";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      formatter.${system} = pkgs.alejandra;
      devShells.${system}.default = pkgs.mkShell {
        name = "nix-nodejs-template";
        packages = [
          # standard toolkit
          pkgs.nodejs # nixpkgs provides a "nodejs" package that corresponds to the current LTS version of nodejs, but you can specify a version (i.e. node_20) if necessary
          pkgs.nodePackages.pnpm # a faster alternative to npm and yarn, with a less adopted toolchain
          pkgs.typescript

          # optionally required by your code editor to lint and format your code
          pkgs.nodePackages.prettier # formatter
          pkgs.nodePackages.eslint # linter
          pkgs.nodePackages.typescript-language-server # lsp

          # example package to serve a static nextjs export
          pkgs.nodePackages.serve

          # nix lsp and formatter
          pkgs.nixd
          pkgs.alejandra
        ];
      };
    };
}
