{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.command-line.atuin;
in
{
  options.command-line.atuin = {
    enable = mkEnableOption "Atuin";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      hyperfine
    ];
    programs.atuin = {
      enable = true;
      # enableBashIntegration = true;
      enableFishIntegration = true;
      enableZshIntegration = true;
      flags = [ "--disable-up-arrow" ];
    };
  };
}
