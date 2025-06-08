{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.command-line.zoxide;
in
{
  options.command-line.zoxide = {
    enable = mkEnableOption "Zoxide";
  };

  config = lib.mkIf cfg.enable {
    programs.zoxide = {
      enable = true;
      enableBashIntegration = true;
    };
  };
}
