{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.command-line.git;
in
{
  options.command-line.git = {
    enable = mkEnableOption "Git";
  };

  config = lib.mkIf cfg.enable {
    programs.git = {
      enable = true;
      userName = "svitax";
      userEmail = "svitaxiom@gmail.com";
      ignores = [
        ".direnv/"
        "result"
      ];
    };
  };
}
