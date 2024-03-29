{ ... }: {
  # use xdg-mime to query the mimetype of a file `xdg-mime query filetype foo.pdf`
  # or to query the default of a mimetype `xdg-mime query default application/epub+zip`
  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = {
    "text/html" = "org.qutebrowser.qutebrowser.desktop";
    "x-scheme-handler/http" = "org.qutebrowser.qutebrowser.desktop";
    "x-scheme-handler/https" = "org.qutebrowser.qutebrowser.desktop";
    "x-scheme-handler/about" = "org.qutebrowser.qutebrowser.desktop";
    "x-scheme-handler/unknown" = "org.qutebrowser.qutebrowser.desktop";
    "application/epub+zip" = [ "sioyek.desktop" ];
  };
}
