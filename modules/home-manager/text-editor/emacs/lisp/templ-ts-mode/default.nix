{
  lib,
  pkgs,
  epkgs,
}:
# let
#   grammarRev = "592faa3186ef857c92e4bd1c31d73c07a4a334db";
#   tree-sitter-templ = pkgs.tree-sitter.buildGrammar {
#     language = "tree-sitter-templ";
#     version = grammarRev;
#     src = pkgs.fetchFromGitHub {
#       owner = "vrischmann";
#       repo = "tree-sitter-templ";
#       rev = grammarRev;
#       sha256 = "sha256-XX1+P8ibo8REYYZQaC47lneg/roralo+YiRwFNnARsQ=";
#     };
#   };
# in
epkgs.melpaBuild {
  pname = "templ-ts-mode";
  version = "20250223.2347";
  src = ./.;
  recipe = pkgs.writeText "recipe" ''
    (templ-ts-mode :fetcher git :url "localhost")
  '';
  # src = pkgs.fetchFromGitHub {
  #   owner = "danderson";
  #   repo = "templ-ts-mode";
  #   rev = "020976f0cf2cf27a1a6e1b59e92c443c52b03a52";
  #   hash = "sha256-yicePCaXWf8xgDsKeLuZ7e5FEqF8F+wc5Xg1Oe21XvE=";
  # };
  # recipe = pkgs.writeText "recipe" ''
  #   (templ-ts-mode :repo "danderson/templ-ts-mode" :fetcher github)
  # '';
  packageRequires = with epkgs; [
    pkgs.tree-sitter-grammars.tree-sitter-templ
    # tree-sitter-templ
  ];
  meta = {
    homepage = "";
    license = lib.licenses.gpl3Plus;
  };
}
