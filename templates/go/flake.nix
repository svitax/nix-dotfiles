{
  description = "A Nix-flake-based Go development environment";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = {nixpkgs, ...} @ inputs: let
    goVersion = 20; # Change this to update the whole stack
    overlays = [(final: prev: {go = prev."go_1_${toString goVersion}";})];
    systems = ["x86_64-linux"];
    forEachSupportedSystem = f:
      nixpkgs.lib.genAttrs systems (system:
        f {
          pkgs = import nixpkgs {inherit overlays system;};
        });
  in {
    formatter = forEachSupportedSystem (pkgs: pkgs.alejandra);
    devShells = forEachSupportedSystem ({pkgs}: {
      default = pkgs.mkShell {
        name = "nix-go-template";
        packages = with pkgs; [
          # go 1.20 (specified by overlay)
          go

          # goimports, godoc, etc.
          gotools

          # https://github.com/golangci/golangci-lint
          golangci-lint
        ];
      };
    });
  };
}
