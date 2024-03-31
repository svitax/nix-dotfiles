;;; popper-extras.el -*- lexical-binding: t; -*-

(defvar popper-echo--propertized-names nil
  "Alist of popup buffer names and their shortened, propertized
display names.")

(defun my/popper-message-shorten (full-name)
  (let ((name (file-name-nondirectory full-name)))
    (or (alist-get name popper-echo--propertized-names nil nil #'string=)
        (let ((short-name
               (cond
                ((string= "*Messages*" name)
                 (concat (propertize "LOG " 'face 'default)
                         (propertize name 'face 'popper-echo-area-buried)))
                ((string-match "^\\*[hH]elpful.*?: \\(.*\\)\\*$" name)
                 (concat (propertize "HLP " 'face '(:inherit link :underline nil))
                         (propertize (match-string 1 name) 'face 'popper-echo-area-buried)))
                ((string-match "^\\*Help:?\\(.*\\)\\*$" name)
                 (concat (propertize "HLP" 'face
                                     '(:inherit link :underline nil))
                         (propertize (match-string 1 name)
                                     'face 'popper-echo-area-buried)))
                ((string-match "^\\*\\(e?\\)shell:? ?\\(.*\\)\\*$" name)
                 (concat (if (string-empty-p (match-string 1 name))
                             (propertize "SH" 'face 'success)
                           (propertize "ESH" 'face 'success))
                         (unless (string-empty-p (match-string 2 name)) " ")
                         (propertize (match-string 2 name)
                                     'face 'popper-echo-area-buried)))
                ((string-match "^\\*\\(.*?\\)-\\(e?\\)shell\\*$" name)
                 (concat (if (string-empty-p (match-string 2 name))
                             (propertize "SH" 'face 'success)
                           (propertize "ESH" 'face 'success))
                         (unless (string-empty-p (match-string 1 name)) " ")
                         (propertize (match-string 1 name)
                                     'face 'popper-echo-area-buried)))
                ((string-match "^[*]?\\(.*?\\) *\\(?:[Oo]utput\\|Command\\)\\*$" name)
                 (concat (propertize "OUT "
                                     'face '(:inherit warning))
                         (propertize (match-string 1 name)
                                     'face 'popper-echo-area-buried)))
                ((string-match "^\\*\\(.*?\\)[ -][Ll]og\\*$" name)
                 (concat (propertize "LOG "
                                     ;; '(:inherit link-visited :underline nil)
                                     'face 'default)
                         (propertize (match-string 1 name)
                                     'face 'popper-echo-area-buried)))
                ((or (string-match "^\\*[Cc]ompil\\(?:e\\|ation\\)\\(.*\\)\\*$" name)
                     (string-match
                      "^\\*\\(.*?\\)[ -]?\\(?:byte\\)?[ -]?[Cc]ompil\\(?:e\\|ation\\)\\*$" name))
                 (concat (propertize "COM "
                                     'face '(:inherit link-visited :underline nil :weight normal))
                         (propertize (match-string 1 name) 'face 'popper-echo-area-buried)))
                ((string-match "^[*]?\\(?:e?shell.*\\|v?term\\).*\\*$" name)
                 (concat (propertize "RPL " 'face 'success) name))
                (t (propertize name 'face 'popper-echo-area-buried)))))
          (cdar (push (cons name short-name) popper-echo--propertized-names))))))

;; (setq popper-mode-line
;;       '(:eval (let ((face (if (doom-modeline--active)
;;			      'doom-modeline-emphasis
;;                             'doom-modeline)))
;;		(if (and (doom-modeline-icon-displayable-p)
;;			 (bound-and-true-p doom-modeline-icon)
;;			 (bound-and-true-p doom-modeline-mode))
;;                     (format " %s " (all-the-icons-octicon "pin" :face face :v-adjust 0.05))
;;		  (propertize " POP " 'face face)))))

(defvar my/occur-grep-modes-list '(occur-mode
                                   grep-mode
                                   xref--xref-buffer-mode
                                   ivy-occur-grep-mode
                                   ivy-occur-mode
                                   locate-mode
                                   flymake-diagnostics-buffer-mode
                                   rg-mode)
  "List of major-modes used in occur-type buffers")

;; This does not work at buffer creation since the major-mode for
;; REPLs is not yet set when `display-buffer' is called, but is
;; useful afterwards
(defvar my/repl-modes-list '(matlab-shell-mode
                             eshell-mode
                             geiser-repl-mode
                             shell-mode
                             eat-mode
                             ;; vterm-mode
                             inferior-python-mode
                             cider-repl-mode
                             fennel-repl-mode
                             jupyter-repl-mode
                             inferior-ess-julia-mode)
  "List of major-modes used in REPL buffers")

(defvar my/repl-names-list
  '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
    "\\*.*REPL.*\\*"
    "\\*MATLAB\\*"
    "\\*Python\\*"
    "^\\*jupyter-repl.*?\\(\\*\\|<[[:digit:]]>\\)$"
    "\\*Inferior .*\\*$"
    "^\\*julia.*\\*$"
    "^\\*cider-repl.*\\*$"
    "\\*ielm\\*"
    "\\*edebug\\*")
  "List of buffer names used in REPL buffers")

(defvar my/help-modes-list '(helpful-mode
                             help-mode
                             pydoc-mode
                             eldoc-mode
                             TeX-special-mode)
  "List of major-modes used in documentation buffers")

(defvar my/man-modes-list '(Man-mode woman-mode)
  "List of major-modes used in Man-type buffers")

(defvar my/message-modes-list '(compilation-mode
                                edebug-eval-mode)
  "List of major-modes used in message buffers")

(setq popper-reference-buffers (append my/help-modes-list
				       my/man-modes-list
				       my/repl-modes-list
				       my/repl-names-list
				       my/occur-grep-modes-list
				       '(Custom-mode
					 compilation-mode
					 messages-buffer-mode)
				       '(("^\\*Warnings\\*$" . hide)
					 ("^\\*Compile-Log\\*$" . hide)
					 "^\\*Matlab Help.*\\*$"
					 ;; "^\\*Messages\\*$"
					 "^\\*Backtrace\\*"
					 "^\\*evil-registers\\*"
					 "^\\*Apropos"
					 "^Calc:"
					 "^\\*eldoc\\*"
					 "^\\*TeX errors\\*"
					 "^\\*ielm\\*"
					 "^\\*TeX Help\\*"
					 "^\\*ChatGPT\\*"
					 "^\\*gptel-quick\\*"
					 "\\*Shell Command Output\\*"
					 ("\\*Async Shell Command\\*" . hide)
					 ("\\*Detached Shell Command\\*" . hide)
					 "\\*Completions\\*"
					 ;; "\\*scratch.*\\*$"
					 "[Oo]utput\\*")))

(provide 'popper-extras)
