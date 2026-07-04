{
  ...
}:
{
  flake.modules.nixos.man =
    { pkgs, ... }:
    {
      documentation.man = {
        enable = true;
        generateCaches = true;
      };
      environment.systemPackages = with pkgs; [
        man-pages
        man-pages-posix
      ];
    };
}
