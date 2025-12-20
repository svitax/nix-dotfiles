let
  modulesDir = ./.;

  # Recursively find all .nix files in subdirectories
  findModules =
    dir:
    let
      entries = builtins.readDir dir;
      processEntry =
        name: type:
        let
          path = dir + "/${name}";
        in
        if type == "directory" then
          findModules path
        else if type == "regular" && builtins.match ".*\\.nix$" name != null && path != ./default.nix then
          [ path ]
        else
          [ ];
    in
    builtins.concatLists (builtins.attrValues (builtins.mapAttrs processEntry entries));
in
{
  allModules = {
    imports = findModules modulesDir;
  };
}
