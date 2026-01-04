{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.shell.notmuch;
in
{
  options.modules.shell.notmuch = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      notmuch
    ];

    home.file.".notmuch-config".source = ./.notmuch-config;
    home.file."mail/.notmuch/hooks/pre-new" = {
      source = ./pre-new;
      executable = true;
    };
    home.file."mail/.notmuch/hooks/post-new" = {
      source = ./post-new;
      executable = true;
    };
  };
}
