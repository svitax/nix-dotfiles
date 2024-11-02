{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.shell.zsh;
in
{
  options.shell.zsh = {
    enable = mkEnableOption "Zshell";
  };

  config = lib.mkIf cfg.enable {
    programs.zsh.enable = true;
    # users.defaultUserShell = pkgs.zsh;
  };
}
