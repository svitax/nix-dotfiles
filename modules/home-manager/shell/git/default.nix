{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.shell.git;
in
{
  options.modules.shell.git = {
    enable = mkEnableOption "Git";
  };

  config = lib.mkIf cfg.enable {
    programs.git = {
      enable = true;
      settings = {
        user.name = "svitax";
        user.email = "svitaxiom@gmail.com";
      };
      ignores = [
        ".direnv/"
        "result"
      ];
    };
  };
}
