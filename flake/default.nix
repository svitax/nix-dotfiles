{ inputs, ... }:
# TODO: add autowiring helpers
# let
#   autowire = import ./lib/autowire.nix { inherit lib; };
#   root = ./.;
# in
{
  imports = [
    ../hosts
  ];

  flake = {
    # TODO: nixosModules = autowire.discoverModules { dir = root + /modules/nixos; };
    nixosModules = import ../modules/nixos;

    # TODO: darwinModules = autowire.discoverModules { dir = root + /modules/darwin; };

    # TODO: homeManagerModules = autowire.discoverModules { dir = root + /modules/home; };
    homeManagerModules = import ../modules/home-manager;

    # Custom packages and modifications, exported as overlays
    overlays = import ../overlays { inherit inputs; };

    # Custom flake templates, accessible through
    # 'nix flake init -t github:username/reponame#template' or
    # 'nix flake init -t local-path-to-repo#template'
    templates = import ../templates;
  };

  # TODO: add necessary system for macos (aarch64-darwin/x86_64darwin?)
  systems = [ "x86_64-linux" ];

  perSystem =
    {
      system,
      pkgs,
      ...
    }:
    {
      # NOTE: look at devenv for how to include certain languages?
      devShells.default = pkgs.mkShell {
        packages = with pkgs; [
          inputs.self.formatter.${system}
          nixfmt
          nixd
        ];
        name = "evermind";
        DIRENV_LOG_FORMAT = "";
      };
      # TODO: set formatter to treefmt-nix
      formatter = pkgs.writeShellApplication {
        name = "lint";
        runtimeInputs = builtins.attrValues {
          inherit (pkgs)
            nixfmt
            deadnix
            statix
            vulnix # TODO integrate vulnix
            fd
            ;
        };
        text = ''
          fd '.*\.nix' . -x statix fix -- {} \;
          fd '.*\.nix' . -X deadnix -e -- {} \; -X nixfmt {} \;
        '';
      };
    };
}
