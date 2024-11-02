{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.shell.bash;
in
{
  options.shell.bash = {
    enable = mkEnableOption "Bash";
  };

  config = lib.mkIf cfg.enable {
    programs.bash = {
      blesh.enable = true;
    };
    # users.defaultUserShell = pkgs.zsh;
  };
}
