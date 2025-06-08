{ inputs, ... }:
let
  inherit (inputs.self) outputs;
in
{
  imports = [
    ../hosts
  ];

  flake = {
    nixosModules = import ../modules/nixos;

    homeManagerModules = import ../modules/home-manager;

    # Custom packages and modifications, exported as overlays
    # overlays = import ../overlays { inherit inputs outputs; };

    # Custom flake templates, accessible through
    # 'nix flake init -t github:username/reponame#template' or
    # 'nix flake init -t local-path-to-repo#template'
    templates = import ../templates;
  };

  systems = [ "x86_64-linux" ];

  perSystem =
    {
      system,
      pkgs,
      ...
    }:
    {
      # TODO: set devShells to devenv
      devShells.default = pkgs.mkShell {
        packages = with pkgs; [
          inputs.self.formatter.${system}
          nixfmt-rfc-style
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
            nixfmt-rfc-style
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
