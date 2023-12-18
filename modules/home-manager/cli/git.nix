{ config, ... }:
let inherit (config.colorScheme) colors;
in {
  programs.git = {
    enable = true;
    userName = "Igor Barale";
    userEmail = "svitaxiom@gmail.com";
    extraConfig = { init.defaultBranch = "master"; };
    ignores = [ ".direnv" "result" ];
    delta = {
      enable = true;
      options = {
        features = "decorations";
        line-numbers = {
          line-numbers-minus-style = "1 bold";
          line-numbers-plus-style = "2 bold";
        };
        decorations = {
          # author: https://github.com/maxfangx
          # General appearance
          dark = true;
          syntax-theme = "gruvbox-dark";
          # File
          file-style = "'#FFFFFF' bold";
          file-added-label = "[+]";
          file-copied-label = "[==]";
          file-modified-label = "[*]";
          file-removed-label = "[-]";
          file-renamed-label = "[->]";
          file-decoration-style = "'#${colors.base0D}' ul";
          # file-decoration-style = "#84786A" ul
          # No hunk headers
          hunk-header-style = "omit";
          # Line numbers
          line-numbers = true;
          line-numbers-left-style = "#${colors.base03}";
          line-numbers-right-style = "#${colors.base03}";
          line-numbers-minus-style = "#c44936";
          line-numbers-plus-style = "#9e9e3c";
          line-numbers-zero-style = "#${colors.base03}";
          line-numbers-left-format = " {nm:>3} │";
          line-numbers-right-format = " {np:>3} │";
          # Diff contents
          inline-hint-style = "syntax";
          minus-style = "syntax '#543834'";
          minus-emph-style = "syntax '#c44936'";
          minus-non-emph-style = "syntax auto";
          plus-style = "syntax '#3f3f29'";
          plus-emph-style = "syntax '#9e9e3c'";
          plus-non-emph-style = "syntax auto";
          whitespace-error-style = "'#${colors.base08}' reverse";
          # Commit hash
          commit-decoration-style = "normal box";
          commit-style = "'#${colors.base05}' bold";
          # Blame
          blame-code-style = "syntax";
          blame-format = "{author:>18} ({commit:>8}) {timestamp:<13} ";
          blame-palette =
            "'#000000' '#${colors.base00}' '#${colors.base01}' '#${colors.base02}'";
          # Merge conflicts
          merge-conflict-begin-symbol = "⌃";
          merge-conflict-end-symbol = "⌄";
          merge-conflict-ours-diff-header-style = "'#${colors.base0A}' bold";
          merge-conflict-theirs-diff-header-style =
            "'#${colors.base0A}' bold overline";
          merge-conflict-ours-diff-header-decoration-style = "";
          merge-conflict-theirs-diff-header-decoration-style = "";
        };
      };
    };
  };
}
