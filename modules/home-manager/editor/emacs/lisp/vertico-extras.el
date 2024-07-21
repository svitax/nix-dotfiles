;;; vertico-extras.el -*- lexical-binding: t; -*-

(defun +vertico-multiform-monocle ()
  (interactive)
  (setq-local vertico-buffer-display-action '(display-buffer-full-frame))
  (vertico-multiform-buffer))

(defun +vertico-multiform-unobtrusive ()
  "Toggle between vertico-unobtrusive/vertico-flat and default vertico."
  (interactive)
  (vertico-multiform-vertical))

(defun +vertico-really-exit ()
  (interactive)
  (if minibuffer--require-match
      (minibuffer-complete-and-exit)
    (exit-minibuffer)))

(provide 'vertico-extras)
;;; vertico-extras.el ends here
