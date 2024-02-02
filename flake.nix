{
  description = "NixOS config";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts.url = "github:hercules-ci/flake-parts";
    nix-colors.url = "github:misterio77/nix-colors";

    nh.url = "github:viperML/nh";
    nh.inputs.nixpkgs.follows = "nixpkgs";

    nix-index-database.url = "github:Mic92/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    firefox-addons.url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
    firefox-addons.inputs.nixpkgs.follows = "nixpkgs";

    firefox-gnome-theme.url = "github:rafaelmardojai/firefox-gnome-theme";
    firefox-gnome-theme.flake = false;

    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # NOTE: Add any other flake you might need
    # nix-nvim.url = "github:svitax/nvim-flake";
    # nvim-fennec.url = "path:./modules/neovim";
    # hardware.url = "github:nixos/nixos-hardware";
    zjstatus.url = "github:dj95/zjstatus";
  };

  outputs = { self, nixpkgs, flake-parts, ... }@inputs:
    let
      inherit (self) outputs;
      # lib = nixpkgs.lib // home-manager.lib;
    in flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      perSystem = { pkgs, system, ... }: {
        # Your custom packages
        # Accessible through 'nix build', 'nix shell', etc
        packages.default = import ./pkgs { inherit pkgs; };

        # Formatter for your nix files, available through 'nix fmt'
        # Other options beside  include 'nixpkgs-fmt and 'alejandra''
        # formatter = pkgs.alejandra;
        formatter = pkgs.writeShellApplication {
          name = "link";
          runtimeInputs = [ pkgs.fd pkgs.nixfmt pkgs.stylua ];
          text = ''
            fd '.*\.nix' . -X nixfmt {} \;
            fd '.*\.lua' . -X stylua {} \;
          '';
        };

        devShells.default = pkgs.mkShell { name = "nix-dotfiles"; };
      };

      flake = {
        # Your custom flake templates
        # Accessible through 'nix flake init -t github:username/reponame#template'
        templates = import ./templates;

        # Your custom packages and modifications, exported as overlays
        overlays = import ./overlays { inherit inputs outputs; };

        # NixOS configuration entrypoint
        # Available through 'nixos-rebuild --flake .#your-hostname'
        nixosConfigurations = {
          # FIXME: replace with your hostname
          nixos = nixpkgs.lib.nixosSystem {
            specialArgs = { inherit inputs outputs; };
            # > Our main nixos configuration file <
            modules = [ ./hosts/fennec/configuration.nix ];
          };
        };
      };
    };
}
