{
  ...
}:
{
  flake.modules.nixos.shiori =
    { pkgs, ... }:
    {
      services.shiori.enable = true;
      environment.systemPackages = [ pkgs.shiori ];
    };
}
