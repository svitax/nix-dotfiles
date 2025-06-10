{
  description = "A Nix-flake-based Python development environment";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, ... }:

    let
      system = "x86_64-linux";
      #       ↑ Swap it for your system if needed
      #       "aarch64-linux" / "x86_64-darwin" / "aarch64-darwin"
      pkgs = nixpkgs.legacyPackages.${system};
    in

    {
      devShells.${system}.default = pkgs.mkShell {
        name = "nix-python-template";

        packages = [
          pkgs.poetry # or pkgs.uv

          pkgs.nixfmt-rfc-style
          pkgs.nixd

          # TODO these should be provided by poetry or uv
          pkgs.jupyter
          pkgs.basedpyright
          pkgs.black
          pkgs.ruff

          # If the dependencies need system libs, you usually need pkg-config + the lib
          # pkgs.pkg-config
          # pkgs.openssl
        ];

      };
    };
}

# {
#   description = "A Nix-flake-based Go development environment";

#   inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

#   outputs = { nixpkgs, ... }@inputs:
#     let
#       goVersion = 24; # Change this to update the whole stack
#       overlays = [ (final: prev: { go = prev."go_1_${toString goVersion}"; }) ];
#       systems = [ "x86_64-linux" ];
#       forEachSupportedSystem = f:
#         nixpkgs.lib.genAttrs systems
#         (system: f { pkgs = import nixpkgs { inherit overlays system; }; });
#     in {
#       formatter = forEachSupportedSystem (pkgs: pkgs.alejandra);
#       devShells = forEachSupportedSystem ({ pkgs }: {
#         default = pkgs.mkShell {
#           name = "nix-go-template";
#           packages = with pkgs; [
#             # go 1.24 (specified by overlay)
#             go
#             gopls

#             # goimports, godoc, etc.
#             gotools
#             go-tools

#             # https://github.com/golangci/golangci-lint
#             golangci-lint
#           ];
#         };
#       });
#     };
# }
