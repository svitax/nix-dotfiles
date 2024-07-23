;;; eldoc-extras.el --- Extra lisp code for Eldoc -*- lexical-binding: t; -*-

;;; Commentary:

;; Add color for Eglot buffers.
;; (defun flymake-eglot-eldoc-function (report-doc &rest _)
;;   "Document diagnostics at point.
;; Intended for `eldoc-documentation-functions'."
;;   (let ((diags (flymake-diagnostics (point))))
;;     (when diags
;;       (funcall report-doc
;; 	       (mapconcat (lambda (d)
;; 			    (let ((level (flymake-diagnostic-type d)))
;; 			      (pcase level
;; 				;; BUG: no color in eldoc window outside of eglot now
;; 				('warning (propertize (flymake-diagnostic-text d) 'face 'flymake-warning-echo))
;; 				('error (propertize (flymake-diagnostic-text d) 'face 'flymake-error-echo))
;; 				('note (propertize (flymake-diagnostic-text d) 'face 'flymake-note-echo))
;; 				('flymake-warning (propertize (flymake-diagnostic-text d) 'face 'flymake-warning-echo))
;; 				('flymake-error (propertize (flymake-diagnostic-text d) 'face 'flymake-error-echo))
;; 				('flymake-note (propertize (flymake-diagnostic-text d) 'face 'flymake-note-echo))
;; 				('eglot-warning (propertize (flymake-diagnostic-text d) 'face 'flymake-warning-echo))
;; 				('eglot-error (propertize (flymake-diagnostic-text d) 'face 'flymake-error-echo))
;; 				('eglot-note (propertize (flymake-diagnostic-text d) 'face 'flymake-note-echo))
;; 				('_ (flymake-diagnostic-text d))))) diags "\n")
;; 	       :echo (mapconcat #'flymake-diagnostic-oneliner
;; 				diags "\n")))))

(defun +eldoc-setup-elisp ()
  "Setup `eldoc-documentation-functions' for `emacs-lisp-mode' buffers."
  (setq-local eldoc-documentation-functions
	      '(flymake-eldoc-function
		elisp-eldoc-funcall
		elisp-eldoc-var-docstring)))

(defun +eldoc-setup-eglot ()
  "Setup `eldoc-documentation-functions' for `eglot-managed-mode' buffers."
  (setq-local eldoc-documentation-functions
	      '(flymake-eldoc-function
		eglot-signature-eldoc-function
		eglot-hover-eldoc-function)))

(provide 'eldoc-extras)
;;; eldoc-extras.el ends here
