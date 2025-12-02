{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.network.stevenblack;
in
{
  options.network.stevenblack = {
    enable = mkEnableOption "stevenblack list";
  };

  config = lib.mkIf cfg.enable {
    # TODO: refactor networking config
    # Setting the hosts file's mode like this allows me to edit it as
    # root. Changes made to it in this way will be discarded when switching
    # configurations.
    environment.etc.hosts.mode = "0644";
    networking.stevenblack = {
      enable = true;
      block = [
        "fakenews"
        "gambling"
        "porn"
        "social"
      ];
    };
    networking.extraHosts = ''
    0.0.0.0 youtube.com
    0.0.0.0 www.youtube.com
    '';
    # If I don't mind having externally fetched resources, I could use a fixed
    # output derivation (FOD) to fetch the data directly
    # extrahostsfromsteve = pkgs.fetchurl {
    #   url = "https://raw.githubusercontent.com/StevenBlack/hosts/v2.3.7/hosts";
    #   sha256 = "sha256-C39FsyMQ3PJEwcfPsYSF7SZQZGA79m6o70vmwyFMPLM=";
    # };
    # networking.extraHosts = '' ${builtins.readFile extraHosts} '';
  };
}
