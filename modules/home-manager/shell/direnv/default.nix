{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.shell.direnv;
in
{
  options.modules.shell.direnv = {
    enable = mkEnableOption "Direnv";
  };

  config = lib.mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv = {
        enable = true;
      };
    };

    # hide_env_diff = true to suppress list of env vars that changed
    xdg.configFile."direnv/direnv.toml".source = ./direnv.toml;
    # but i actually don't want direnv to produce any output at all
    # (other than error messages)
    home.sessionVariables = {
      DIRENV_LOG_FORMAT = "";
    };
  };
}
