;;; pcmpl-args-extras.el -*- lexical-binding: t; -*-

(defun my/pcmpl-args-prepare ()
  (let ((pfunc
	 (thread-first
	   "pcomplete/"
	   (concat (car (pcomplete-parse-arguments)))
	   (intern))))
    (unless (fboundp pfunc)
      (defalias pfunc 'pcmpl-args-pcomplete-on-man)))
  (list nil :exclusive 'no))

(defun my/pcmpl-args-capf-ensure ()
  (add-hook 'completion-at-point-functions
	    'my/pcmpl-args-prepare -90 t))

(defun my/pcmpl-args-eshell-settings ()
  (setq-local pcomplete-try-first-hook
	      '(eshell-complete-host-reference
		eshell-complete-history-reference
		eshell-complete-user-reference
		eshell-complete-variable-assignment
		eshell-complete-variable-reference
		eshell-complete-lisp-symbols
		t)))

(provide 'pcmpl-args-extras)
