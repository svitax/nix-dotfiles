{
  imports = [
    ../hosts
    #   ../modules
    #	../packages
    #	../templates
  ];

  flake = {
    nixosModules = import ../modules/nixos;
    homeManagerModules = import ../modules/home-manager;
  };

  systems = ["x86_64-linux"];

  perSystem = {
    config,
    pkgs,
    ...
  }: {
    # TODO: set devShells to devenv
    devShells.default = pkgs.mkShell {
      packages = with pkgs; [
        alejandra
        deadnix
        git
        nil
        statix
        vulnix
      ];

      name = "evermind";
      DIRENV_LOG_FORMAT = "";
    };
    # TODO: set formatter to treefmt-nix
    formatter = pkgs.alejandra;
  };
}
