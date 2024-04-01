;;; apheleia-extras.el -*- lexical-binding: t; -*-

(require 'apheleia)

;; Sometimes we want to use `eglot-format-buffer' as the command to format the
;; buffer on save. The benefit of going through `eglot' rather than using the
;; formatter defined by Apheleia is two-fold. A language server's persistance
;; means that formats of the same project are reasonably fast in languages with
;; high JIT latency, like Julia. Adding a hook to `eglot-managed-mode' to call
;; eglot-format-buffer locks up Emacs while the formatter runs. Aphelia's
;; approach of running it in the background is much nicer.
(require 'cl-lib)
(cl-defun apheleia-indent-eglot-managed-buffer
    (&key buffer scratch callback &allow-other-keys)
  "Copy BUFFER to SCRATCH, then format scratch, then call CALLBACK."
  (with-current-buffer scratch
    (setq-local eglot--cached-server
                (with-current-buffer buffer
                  (eglot-current-server)))
    (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
      (eglot-format-buffer))
    (funcall callback)))

(add-to-list 'apheleia-formatters
             '(eglot-managed . apheleia-indent-eglot-manager-buffer))

(defvar apheleia-eglot-managed-modes
  '(julia-mode julia-ts-mode rust-mode rust-ts-mode))

(dolist (mode apheleia-eglot-managed-modes)
  (add-to-list 'apheleia-mode-alist '(mode . eglot-managed)))

;; Replace black with ruff in Python
(setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
(setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff))

;; Add nix-ts-mode
(when-let ((formatter (assoc 'nix-mode apheleia-mode-alist)))
  (setcar formatter 'nix-ts-mode))

(provide 'apheleia-extras)
