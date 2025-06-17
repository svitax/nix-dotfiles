# scuffed way of dynamically generating an attrset for templates so new
# templates will automically be included without having to manually update this
# file
# TODO find a cleaner way to do this?

# nix flake init --template ~/nix-dotfiles#python-script
let
  templatesDir = ./.;
  dirs = builtins.attrNames (builtins.readDir templatesDir);
  isDirectory = name: (builtins.getAttr name (builtins.readDir templatesDir)) == "directory";

  mkTemplate = name: {
    inherit name;
    value = {
      path = templatesDir + "/${name}";
      description = "";
    };
  };
in
builtins.listToAttrs (builtins.map mkTemplate (builtins.filter isDirectory dirs))
