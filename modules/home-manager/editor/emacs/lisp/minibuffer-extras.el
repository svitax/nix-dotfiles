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

(provide 'minibuffer-extras)
;;; minibuffer-extras.el ends here
