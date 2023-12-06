{
  description = "NixOS config";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager = {
      # url = "github:nix-community/home-manager/release-23.05";
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts.url = "github:hercules-ci/flake-parts";
    nix-colors.url = "github:misterio77/nix-colors";
    zjstatus.url = "github:dj95/zjstatus";

    nh.url = "github:viperML/nh";
    nh.inputs.nixpkgs.follows = "nixpkgs";

    nix-index-database.url = "github:Mic92/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    firefox-addons.url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
    firefox-addons.inputs.nixpkgs.follows = "nixpkgs";

    firefox-gnome-theme.url = "github:rafaelmardojai/firefox-gnome-theme";
    firefox-gnome-theme.flake = false;

    # TODO: Add any other flake you might need
    # hardware.url = "github:nixos/nixos-hardware";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    flake-parts,
    ...
  } @ inputs: let
    inherit (self) outputs;
    # lib = nixpkgs.lib // home-manager.lib;
  in
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      perSystem = {
        pkgs,
        system,
        ...
      }: {
        # Your custom packages
        # Accessible through 'nix build', 'nix shell', etc
        packages.default = import ./pkgs {inherit pkgs;};

        # Formatter for your nix files, available through 'nix fmt'
        # Other options beside 'alejandra' include 'nixpkgs-fmt'
        formatter = pkgs.alejandra;

        devShells.default = pkgs.mkShell {
          name = "nix-dotfiles";
          # packages = with pkgs; [];
        };
      };

      flake = {
        # Your custom flake templates
        # Accessible through 'nix flake init -t github:username/reponame#template'
        templates = import ./templates;

        # Your custom packages and modifications, exported as overlays
        overlays = import ./overlays {inherit inputs outputs;};

        # NixOS configuration entrypoint
        # Available through 'nixos-rebuild --flake .#your-hostname'
        nixosConfigurations = {
          # FIXME: replace with your hostname
          nixos = nixpkgs.lib.nixosSystem {
            specialArgs = {inherit inputs outputs;};
            # > Our main nixos configuration file <
            modules = [./nixos/configuration.nix];
          };
        };
      };
    };
}
