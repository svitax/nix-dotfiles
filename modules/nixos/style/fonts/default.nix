{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.style.fonts;
in
{
  options.style.fonts = {
    enable = mkEnableOption "Fonts";
  };

  config = lib.mkIf cfg.enable {
    fonts = {
      enableDefaultPackages = true;
      packages = with pkgs; [
        aporetic
        mplus-outline-fonts.githubRelease
        # babelstone-han
        # jigmo
        # noto-fonts
        # noto-fonts-cjk-sans
        # noto-fonts-emoji
        nerd-fonts.symbols-only
      ];
    };
    fonts.fontconfig = {
      enable = true;
      # useEmbeddedBitmaps = true;
      defaultFonts = {
        serif = [ "Aporetic Serif" "M PLUS 1" ];
        sansSerif = [ "Aporetic Sans" "M PLUS 1" ];
        monospace = [ "Aporetic Sans Mono" ];
      };
    };
    fonts.fontDir.enable = true;
  };
}
