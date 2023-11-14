{
  lib,
  writeShellApplication,
}:
(writeShellApplication {
  name = "live-grep";
  text = builtins.readFile ./live-grep.sh;
})
// {
  meta = with lib; {
    licenses = licenses.mit;
    platforms = platforms.all;
  };
}
