{pkgs, ...}: {
  xdg.configFile."lf/icons".source = ../.config/lf/icons;
  xdg.configFile."pistol/pistol.conf".source = ../.config/lf/pistol.conf;
  programs.lf = {
    enable = true;
    settings = {
      icons = true;
      ignorecase = true;
      preview = true;
      # hidden = true;
      drawbox = true;
    };
    commands = {
      open =
        # bash
        ''
          ''${{
            case $(${pkgs.file}/bin/file --mime-type "$f" -bL) in
              text/*|application/json) $EDITOR "$f";;
              *) xdg-open "$f" ;;
            esac
          }}
        '';
      trash =
        # bash
        ''
          ''${{
            files=$(printf "$fx" | tr '\n' ';')
            while [ "$files" ]; do
              file=''${files%%;*}
              ${pkgs.trash-cli}/bin/trash-put "$(basename "$file")"
              if [ "$files" = "$file" ]; then
                files=""
              else
                files="''${files#*;}"
              fi
            done
          }}
        '';
      clear_trash = ''${pkgs.trash-cli}/bin/trash-empty'';
      restore_trash = ''${pkgs.trash-cli}/bin/trash-restore'';
      fzf_jump =
        # bash
        ''
          ''${{
            res="$(${pkgs.fd}/bin/fd -d 1 --hidden --exclude=.git | ${pkgs.fzf}/bin/fzf --prompt='Jump to location> ' --multi --ansi | sed 's/\\/\\\\/g;s/"/\\"/g')"

            if [ -d "$res" ]; then
              cmd="cd"
            else
              cmd="select"
            fi
            lf -remote "send $id $cmd \"$res\""
          }}
        '';
      fzy =
        # bash
        ''
          ''${{
            path=$(ls -a1 | ${pkgs.fzy}/bin/fzy -l "$(( $(tput lines) - 1 ))")
            [ -z "$path" ] && exit
            [ -d "$path" ] && cmd='cd' || cmd='select'
            lf -remote "send $id $cmd '$path'"
          }}
        '';
      dragon-out = ''%${pkgs.xdragon}/bin/xdragon -a -x "$fx"'';
      editor-open = ''$$EDITOR $f'';
    };
    keybindings = {
      "\\\"" = "";

      "." = "set hidden!";

      l = "open";
      "<enter>" = "open";

      a = "push %touch<space>";
      A = "push %mkdir<space>";

      c = "clear";

      d = "";
      do = "dragon-out";
      "dd" = "cut";
      "dr" = "restore_trash";
      "dx" = "trash";
      "dc" = "clear_trash";

      y = "";
      "yy" = "copy";
      "yp" = "yank_path";
      "yn" = "yank_name";
      "yd" = "yank_dir";

      p = "paste";
      P = ":link";

      "/" = "fzf_jump";

      "`" = "mark-load";
      "\\'" = "mark-load";

      "g~" = "cd";
      gh = "cd";
      "g/" = "/";

      ee = "editor-open";
      V = ''''$${pkgs.bat}/bin/bat --paging=always --theme=gruvbox-dark-hard "$f"'';
    };
    extraConfig = let
      previewer = pkgs.writeShellScriptBin "pv.sh" ''
        file=$1
        w=$2
        h=$3
        x=$4
        y=$5

        if [[ "$( ${pkgs.file}/bin/file -Lb --mime-type "$file")" =~ ^image ]]; then
          ${pkgs.kitty}/bin/kitty +kitten icat --silent --stdin no --transfer-mode file --place "''${w}x''${h}@''${x}x''${y}" "$file" < /dev/null > /dev/tty
          exit 1
        fi

        ${pkgs.pistol}/bin/pistol "$file"
      '';
      cleaner = pkgs.writeShellScriptBin "clean.sh" ''
        ${pkgs.kitty}/bin/kitty +kitten icat --clear --stdin no --silent --transfer-mode file < /dev/null > /dev/tty
      '';
    in ''
      set cleaner ${cleaner}/bin/clean.sh
      set previewer ${previewer}/bin/pv.sh
    '';
  };
}
