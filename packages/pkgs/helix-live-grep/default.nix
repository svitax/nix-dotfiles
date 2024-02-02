{ lib, writeShellApplication, }:
(writeShellApplication {
  name = "helix-live-grep";
  text = builtins.readFile ./helix-live-grep.sh;
})
