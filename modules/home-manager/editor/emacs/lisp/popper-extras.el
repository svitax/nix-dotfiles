;;; popper-extras.el -*- lexical-binding: t; -*-

;; TODO: potentially put these mode lists in buffer-extras to use with display-buffer-alist
(defvar +help-modes-list '(helpful-mode
			   help-mode
			   "^\\*eldoc"
			   apropos-mode)
  "List of major modes used in documentation buffers.")

(defvar +man-modes-list '(Man-mode
			  woman-mode)
  "List of major modes used in Man-type buffers.")

;; This does not work at buffe creation since the major-mode for REPLs is not
;; yet set when `display-buffer' is called, but it is useful afterwards.
(defvar +repl-modes-list '(matlab-shell-mode
			   geiser-repl-mode
			   inferior-python-mode
			   cider-repl-mode
			   fennel-repl-mode
			   jupyter-repl-mode
			   inferior-ess-julia-mode
			   eshell-mode
			   shell-mode
			   eat-mode
			   vterm-mode)
  "List of major modes used in REPL modes.")

(defvar +repl-names-list '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
			   "\\*.*REPL.*\\*"
			   "\\*MATLAB\\*"
			   "\\*Python\\*"
			   "^\\*jupyter-repl.*?\\(\\*\\|<[[:digit:]]>\\)$"
			   "\\*Inferior .*\\*$"
			   "^\\*julia.*\\*$"
			   "^\\*cider-repl.*\\*$"
			   "\\*ielm\\*"
			   "\\*edebug\\*")
  "List of buffer names used in REPL buffers.")

;; TODO: should i add embark-collect-mode?
(defvar +occur-grep-modes-list '(occur-mode
				 grep-mode
				 xref--xref-buffer-mode
				 locate-mode
				 flymake-diagnostics-buffer-mode
				 rg-mode)
  "List of major modes used in occur-type buffers.")

(defvar +message-modes-list '(compilation-mode
			      messages-buffer-mode
			      edebug-eval-mode
			      )
  "List of major modes used in message buffers.")

(defvar +shell-command-names-list '("\\*Shell Command Output\\*"
				    "\\*Async Shell Command\\*"
				    "\\*Detached Shell Command\\*" ))

(defvar +popper-reference-buffers
  (append +help-modes-list
	  +man-modes-list
	  +repl-modes-list
	  +repl-names-list
	  +occur-grep-modes-list
	  +message-modes-list
	  '(("^\\*Warnings\\*$" . hide)
	    "[Oo]utput\\*$"
	    "\\*Completions\\*")))

(provide 'popper-extras)
;;; popper-extras.el ends here
