{
  description = "A Nix-flake-based RUst development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { nixpkgs, rust-overlay, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ rust-overlay.overlays.default ];
      };
      toolchain = pkgs.rust-bin.fromRustupToolchainFile ./toolchain.toml;
    in {
      formatter = pkgs.alejandra;
      devShells.${system}.default = pkgs.mkShell {
        name = "nix-rust-template";
        packages = [
          toolchain
          # We want the unwrapped version, "rust-analyzer" (wrapped) comes with nixpkgs' toolchain
          pkgs.rust-analyzer-unwrapped
          pkgs.vscode-extensions.vadimcn.vscode-lldb.adapter # debugging adapter
          pkgs.taplo # toml language server
        ];
        env = {
          RUST_BACKTRACE = "full";
          RUST_SRC_PATH = "${toolchain}/lib/rustlib/src/rust/library";
        };
      };
    };
}
