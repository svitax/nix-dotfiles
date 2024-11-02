;;; minibuffer-extras.el -*- lexical-binding: t; -*-

;; Add prompt indicator to `completing-read-multiple'. We display
;; [`completing-read-multiple': <separator>], e.g.,
;; [`completing-read-multiple': ,] if the separator is a comma. This is adapted
;; from the README of the `vertico' package by Daniel Mendler. I made some small
;; tweaks to propertize the segments of the prompt.
(defun +crm-indicator (args)
  (cons (format "[`completing-read-multiple': %s]  %s"
		(propertize
		 (replace-regexp-in-string
		  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		  crm-separator)
		 'face 'error)
		(car args))
	(cdr args)))

;; BUG trying to add font-locking to eval-expression
(defvar +read-expression-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map read-expression-map)
    (define-key map "\C-g" #'abort-minibuffers)
    (define-key map [up] nil)
    (define-key map [down] nil)
    (define-key map "\C-j" #'newline)
    (define-key map "\r" #'exit-minibuffer)
    (define-key map "\n" #'exit-minibuffer)
    map))

(defun +read-expression-setup-minibuffer ()
  (let ((inhibit-read-only t))
    ;; Without this the prompt would be painted like code
    (put-text-property 1 (minibuffer-prompt-end) 'font-lock-face 'minibuffer-prompt))
  (emacs-lisp-mode)
  (use-local-map +read-expression-map)
  (setq font-lock-mode t)
  (funcall font-lock-function 1)
  (goto-char (minibuffer-prompt-end))
  (when (looking-at ".*\n")
    (indent-sexp))
  (goto-char (point-max)))

(defun +read-expression (prompt &optional initial-contents hist default read map)
  "Like `read-expression' but using emacs-lisp-mode."
  (minibuffer-with-setup-hook #'+read-expression-setup-minibuffer
    (read-from-minibuffer prompt initial-contents
			  (or map +read-expression-map) read
			  (or hist 'read-expression-history) default)))

(provide 'minibuffer-extras)
;;; minibuffer-extras.el ends here
