;;; keycast-extras.el --- . -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(define-minor-mode druid-modeline-keycast-mode
  "Show current command and its key binding in the mode line."
  :global t
  (if druid-modeline-keycast-mode
      (progn
	(add-hook 'pre-command-hook 'keycast--update t)
	(add-to-list 'global-mode-string '("" keycast-mode-line)))
    (remove-hook 'pre-command-hook 'keycast--update)
    (setq global-mode-string (remove '("" keycast-mode-line) global-mode-string))))

(provide 'keycast-extras)
;;; keycast-extras.el ends here
