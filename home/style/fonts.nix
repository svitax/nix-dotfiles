{ pkgs, ... }: {
  fonts.fontconfig = { enable = true; };
  home.packages = with pkgs; [
    noto-fonts
    iosevka-comfy.comfy
    iosevka-comfy.comfy-duo
    iosevka-comfy.comfy-fixed
    iosevka-comfy.comfy-motion
    # Nerd Fonts
    (nerdfonts.override {
      fonts = [ "JetBrainsMono" "NerdFontsSymbolsOnly" "IosevkaTermSlab"];
    })
  ];
}
