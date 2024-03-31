;;; citar-extras.el -*- lexical-binding: t; -*-

(require 'citar)
(require 'nerd-icons)

(defvar citar-indicator-files-icons
  (citar-indicator-create
   :symbol (nerd-icons-faicon "nf-fa-file" :face 'nerd-icons-green)
   :function #'citar-has-files
   ;; :padding "  " ; need this because the default padding is too low for these icons
   :tag "has:files"))

(defvar citar-indicator-links-icons
  (citar-indicator-create
   :symbol (nerd-icons-faicon "nf-fa-link" :face 'nerd-icons-orange)
   :function #'citar-has-links :padding "  " :tag "has:links"))

(defvar citar-indicator-notes-icons
  (citar-indicator-create
   :symbol (nerd-icons-octicon "nf-oct-note" :face 'nerd-icons-blue)
   :function #'citar-has-notes :padding "  " :tag "has:notes"))

(defvar citar-indicator-cited-icons
  (citar-indicator-create
   :symbol (nerd-icons-faicon "nf-fa-circle" :face 'nerd-icons-green)
   :function #'citar-is-cited
   :padding "  " :tag "is:cited"))

(setq citar-indicators
      (list citar-indicator-files-icons ; citar-indicator-links-icons
	    citar-indicator-notes-icons ; citar-indicator-cited-icons
	    ))

(provide 'citar-extras)
