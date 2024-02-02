{ ... }: {
  # use xdg-mime to query the mimetype of a file `xdg-mime query filetype foo.pdf`
  # or to query the default of a mimetype `xdg-mime query default application/epub+zip`
  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = {
    "application/epub+zip" = [ "sioyek.desktop" ];
  };
}
