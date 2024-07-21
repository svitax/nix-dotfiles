;;; minibuffer-extras.el -*- lexical-binding: t; -*-

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun +crm-indicator (args)
  (cons (format "[CRM%s] %s"
		(replace-regexp-in-string
		 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		 crm-separator)
		(car args))
	(cdr args)))

(provide 'minibuffer-extras)
;;; minibuffer-extras.el ends here
