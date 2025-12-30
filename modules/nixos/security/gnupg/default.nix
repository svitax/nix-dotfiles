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
      default = 3600; # 1 hr
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
