;;; dired-extras.el -*- lexical-binding: t; -*-

(require 'dired)

(defvar my/dired--limit-hist '()
  "Minibuffer history for `my/dired-limit-regexp'.")

(defun my/dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
	     (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
	     "matching PATTERN: ")
     nil 'my/dired--limit-hist)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines)
  (add-to-history 'my/dired--limit-hist regexp))

(provide 'dired-extras)
