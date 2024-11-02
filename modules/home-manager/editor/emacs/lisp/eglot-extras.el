;; eglot-extras.el -*- lexical-binding: t; -*-

(defun +add-eglot-hooks (mode-list)
  "Add `eglot-ensure' to modes in MODE-LIST.

The mode must be loaded, i.e. found with `fboundp'. A mode which is not loaded
will not have a hook added, in which case add it manually with something like
this:

`(add-hook 'some-mode-hook #'eglot-ensure)"
  (dolist (mode-def mode-list)
    (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
      (cond
       ((listp mode) (+add-eglot-hooks mode))
       (t
	(when (and (fboundp mode)
		   (not (eq 'clojure-mode mode)) ;; prefer cider
		   (not (eq 'lisp-mode mode)) ;; prefer sly/slime
		   (not (eq 'scheme-mode mode)) ;; prefer geiser
		   )
	  (let ((hook-name (format "%s-hook" (symbol-name mode))))
	    (add-hook (intern hook-name) #'eglot-ensure))))))))

;; add eglot-ensure to all modes that offer eglot/lsp support
(defun +eglot-auto-ensure-all ()
  "Add `eglot-ensure' to major modes that offer LSP support.

Major modes are only selected if the major mode's associated LSP binary is
detected on the system."
  (when (require 'eglot nil :noerror)
    (+add-eglot-hooks eglot-server-programs)))

;; eglot integration for treesitter modes
(defun +eglot-add-tree-sitter-modes ()
  "Add accompanying tree-sitter major modes to `eglot-server-programs'."
  (when (and (require 'eglot nil :noerror)
	     (require 'treesit-auto nil :noerror))
    (+eglot--add-tree-sitter-modes eglot-server-programs)))

(defun +eglot--add-tree-sitter-modes (mode-list)
  (dolist (it mode-list)
    (when-let* ((tree-sitter-mode-remap (treesit-auto--build-major-mode-remap-alist))
		(mode (car it))
		(new-modes
		 (cond
		  ((symbolp mode)
		   (when-let ((ts-variant (alist-get mode tree-sitter-mode-remap)))
		     (list mode ts-variant)))
		  ((consp mode)
		   (when-let ((ts-variants
			       (cl-loop
				for sub-mode in mode
				with ts-variant = nil
				do (setq ts-variant
					 (alist-get sub-mode tree-sitter-mode-remap))
				when ts-variant collect ts-variant)))
		     (append mode ts-variants))))))
      (setcar it new-modes))))

;; Eglot automatically adds `eglot--mode-line-format' to `mode-line-misc-info'
;; I don't like that. Let's remove it.
(defun +eglot-remove-mode-line-misc-info ()
  (setq mode-line-misc-info
	(delete '(eglot--managed-mode (" [" eglot--mode-line-format "] "))
		mode-line-misc-info)))

(provide 'eglot-extras)
;; eglot-extras.el ends here
