;; subword-extras.el -*- lexical-binding: t; -*-

;; From	http://stackoverflow.com/questions/6133799/delete-a-word-without-adding-it-to-the-kill-ring-in-emacs
(defun +backward-delete-subword (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do it that many times."
  (interactive "p")
  (delete-region (point)
		 (progn
		   (subword-backward arg)
		   (point))))


(provide 'subword-extras)
;; subword-extras.el ends here
