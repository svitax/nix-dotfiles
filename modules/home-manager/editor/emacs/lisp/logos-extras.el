;;; logos-extras.el --- . -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +logos-olivetti ()
  "Toggle `olivetti-mode'."
  (when (and logos-focus-mode (require 'olivetti nil t))
    ;; (logos-set-buffer-local-value 'visual-fill-column-center-text t)
    ;; Toggle off `virtual-auto-fill-mode' because it conflicts.
    (logos-set-mode-arg 'virtual-auto-fill-mode -1)
    (logos-set-mode-arg 'olivetti-mode 1)))

(defun +logos-recenter-top ()
  "Use `recenter' to reposition the view at the top."
  (unless (derived-mode-p 'prog-mode)
    (recenter 0)))

(defun +logos-reveal-entry ()
  "Reveal Org or Outline entry."
  (cond
   ((and (eq major-mode 'org-mode)
	 (org-at-heading-p))
    ;; If you prefer to reveal the entire subtree, replace with
    ;; (org-show-subtree)
    (org-fold-show-entry))
   ((or (eq major-mode 'outline-mode)
	(bound-and-true-p outline-minor-mode))
    (outline-show-entry))))

(provide 'logos-extras)
;;; logos-extras.el ends here
