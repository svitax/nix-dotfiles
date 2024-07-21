;;; embark-extras.el -*- lexical-binding: t; -*-

(defun +embark-on-last-message (arg)
  "Act on the last message displayed in the echo area."
  (interactive "P")
  (with-current-buffer "*Messages*"
    (goto-char (1- (point-max)))
    (embark-act arg)))

(defun +embark-select ()
  (interactive)
  (prog1 (embark-select)
    (if (minibufferp)
	(when (bound-and-true-p vertico-mode)
	  (vertico-next))
      (call-interactively #'next-line))))

(provide 'embark-extras)
;;; embark-extras.el ends here
