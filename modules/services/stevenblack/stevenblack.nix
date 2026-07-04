{
  ...
}:
{
  flake.modules.nixos.stevenblack = {
    environment.etc.hosts.mode = "0644";
    networking.stevenblack = {
      enable = true;
      block = [
        "fakenews"
        "gambling"
        "porn"
        "social"
      ];
    };
  };
}
