{
  pkgs,
  config,
  ...
}: {
  home.packages = with pkgs; [
    papis
  ];
  home.file.".config/papis/config".text =
    # ini
    ''
      [papers]
      dir=${config.home.homeDirectory}/Documents/papers

      [settings]
      add-file-name={doc[year]}-{doc[author_list][0][family]}-{doc[title]}
      add-folder-name={doc[year]}-{doc[author_list][0][family]}-{doc[title]}
      default-library=papers
      file-browser=lf
      notes-name=notes.md
      opentool=xdg-open
      picktool=fzf
      ref-format={doc[author_list][0][family]}{doc[year]}
      unique-document-keys = [ 'file' ]
    '';
}
