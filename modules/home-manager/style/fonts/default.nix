{ pkgs, ... }:
{
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    iosevka-comfy.comfy
    iosevka-comfy.comfy-duo
    cozette
    (nerdfonts.override {
      fonts = [
        "NerdFontsSymbolsOnly"
        "Iosevka"
      ];
    })
  ];
}
