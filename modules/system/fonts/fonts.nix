{
  ...
}:
{
  flake.modules.nixos.fonts =
    { pkgs, ... }:
    {
      fonts = {
        enableDefaultPackages = true;
        packages = with pkgs; [
          aporetic
          mplus-outline-fonts.githubRelease
          nerd-fonts.symbols-only
        ];
        fontconfig = {
          enable = true;
          defaultFonts = {
            serif = [ "Aporetic Serif" "M PLUS 1" ];
            sansSerif = [ "Aporetic Sans" "M PLUS 1" ];
            monospace = [ "Aporetic Sans Mono" ];
          };
          antialias = true;
        };
        fontDir.enable = true;
      };
    };

  flake.modules.darwin.fonts =
    { pkgs, ... }:
    {
      fonts.packages = with pkgs; [
        aporetic
        mplus-outline-fonts.githubRelease
        nerd-fonts.symbols-only
      ];
    };
}
