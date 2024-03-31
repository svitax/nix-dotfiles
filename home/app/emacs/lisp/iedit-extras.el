;;; iedit-extras.el -*- lexical-binding: t; -*-

(defun my/iedit-1-down (arg)
  (interactive "p")
  (let ((current-prefix-arg '(1)))
    (call-interactively #'iedit-mode)
    (iedit-expand-down-to-occurrence)))

(defun my/iedit-1-up (arg)
  (interactive "p")
  (let ((current-prefix-arg '(1)))
    (call-interactively #'iedit-mode)
    (iedit-expand-up-to-occurrence)))

(provide 'iedit-extras)
