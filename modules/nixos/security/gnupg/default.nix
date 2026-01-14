{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.modules.security.gnupg;
in
{
  options.modules.security.gnupg = {
    enable = mkEnableOption "gnupg";
    cacheTTL = mkOption {
      type = types.int;
      default = 43200; # 12 hrs
    };
  };

  config = lib.mkIf cfg.enable {
    programs.gnupg = {
      agent = {
        enable = true;
        pinentryPackage = pkgs.pinentry-gtk2;
        settings = {
          default-cache-ttl = cfg.cacheTTL;
          allow-emacs-pinentry = "";
          allow-loopback-pinentry = "";
        };
      };
    };
  };
}
