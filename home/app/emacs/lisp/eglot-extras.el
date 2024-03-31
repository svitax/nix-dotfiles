;;; eglot-extras.el -*- lexical-binding: t; -*-

(require 'eglot)

;; Eglot automatically adds `eglot--mode-line-format' to `mode-line-misc-info'
;; I don't like that. Let's remove it.
(defun my/eglot-remove-mode-line-misc-info ()
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] "))
                mode-line-misc-info)))

;; Eglot specifically alters the `eldoc-documentation-strategy', so we override
;; it with a hook.
(defun my/eglot-eldoc-settings ()
  (setq eldoc-documentation-strategy
        'eldoc-documentation-compose-eagerly))

(defun my/add-eglot-hooks (mode-list)
  "Add `eglot-ensure' to modes in MODE-LIST.

The mode must be loaded, i.e. found with `fboundp'. A mode which
is not loaded will not have a hook added, in which case add it
manually with something like this:

`(add-hook 'some-mode-hook #'eglot-ensure)'"
  (dolist (mode-def mode-list)
    (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
      (cond
       ((listp mode) (my/add-eglot-hooks mode))
       (t
        (when (and (fboundp mode)
                   (not (eq 'clojure-mode mode)) ; prefer cider
                   (not (eq 'lisp-mode mode))    ; prefer sly/slime
                   (not (eq 'scheme-mode mode))  ; prefer geiser
                   )
          (let ((hook-name (format "%s-hook" (symbol-name mode))))
            (message "adding eglot to %s" hook-name)
            (add-hook (intern hook-name) #'eglot-ensure))))))))

(defun my/eglot-auto-ensure-all ()
  "Add `eglot-ensure' to major modes that offer LSP support.

Major modes are only selected if the major mode's associated LSP
binary is detected on the system."
  (when (require 'eglot nil :noerror)
    (my/add-eglot-hooks eglot-server-programs)))

;; Ask Eglot to stay away from completely taking over Flymake.
;; Just add it as another item.
(add-to-list 'eglot-stay-out-of 'flymake)
(add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)

(provide 'eglot-extras)
