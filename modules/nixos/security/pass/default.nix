{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.security.pass;
in
{
  options.modules.security.pass = {
    enable = mkEnableOption "Password store";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      pass-nodmenu
      # (pass-nodmenu.withExtensions (exts: [
      #   exts.pass-otp
      #   exts.pass-import
      # ]))
    ];
  };
}
