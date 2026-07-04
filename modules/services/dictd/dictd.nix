{
  ...
}:
{
  flake.modules.nixos.dictd =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.dict ];
      environment.etc."dict.conf".text = "server localhost";
      services.dictd = {
        enable = true;
        DBs = with pkgs; [ dict-gcide ];
      };
    };
}
