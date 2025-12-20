{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.shell.zoxide;
in
{
  options.modules.shell.zoxide = {
    enable = mkEnableOption "Zoxide";
  };

  config = lib.mkIf cfg.enable {
    programs.zoxide = {
      enable = true;
      enableBashIntegration = true;
    };
  };
}
