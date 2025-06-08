{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.command-line.buku;
in
{
  options.command-line.buku = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      buku
    ];
  };
}
