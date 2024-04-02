{ inputs, ... }: {
  # colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-hard;
  # scheme: "Gruvbox dark, hard"
  # author: "Dawid Kurek (dawikur@gmail.com), morhetz (https://github.com/morhetz/gruvbox)"
  # base00: "#1d2021" # ----
  # base01: "#3c3836" # ---
  # base02: "#504945" # --
  # base03: "#665c54" # -
  # base04: "#bdae93" # +
  # base05: "#d5c4a1" # ++
  # base06: "#ebdbb2" # +++
  # base07: "#fbf1c7" # ++++
  # base08: "#fb4934" # red
  # base09: "#fe8019" # orange
  # base0A: "#fabd2f" # yellow
  # base0B: "#b8bb26" # green
  # base0C: "#8ec07c" # aqua/cyan
  # base0D: "#83a598" # blue
  # base0E: "#d3869b" # purple
  # base0F: "#d65d0e" # brown
  colorScheme = {
    slug = "modus-vivendi";
    name = "Modus Vivendi";
    author = "Protesilaos Stavrou (https://protesilaos.com)";
    palette = {
      base00 = "#000000";
      base01 = "#1e1e1e";
      base02 = "#303030";
      base03 = "#535353";
      base04 = "#646464";
      base05 = "#ffffff";
      base06 = "#ffffff";
      base07 = "#ffffff";
      base08 = "#ff5f59";
      base09 = "#db7b5f";
      base0A = "#d0bc00";
      base0B = "#44bc44";
      base0C = "#00d3d0";
      base0D = "#2fafff";
      base0E = "#feacd0";
      base0F = "#c0965b";
    };
  };
  # colorScheme = {
  #   slug = "arbutus";
  #   name = "Arbutus";
  #   author = "Protesilaos Stavrou (https://protesilaos.com)";
  #   palette = {
  #     base00 = "#ffead8";
  #     base01 = "#f0d8cf";
  #     base02 = "#e7d2cb";
  #     base03 = "#c7b2ab";
  #     base04 = "#393330";
  #     base05 = "#6e678f";
  #     base06 = "#8a5f4a";
  #     base07 = "#40231f";
  #     base08 = "#b0000f";
  #     base09 = "#b44405";
  #     base0A = "#906200";
  #     base0B = "#007000";
  #     base0C = "#3f69af";
  #     base0D = "#375cc6";
  #     base0E = "#a23ea4";
  #     base0F = "#8a6340";
  #   };
  # };
}
