;;; marginalia-extras.el -*- lexical-binding: t; -*-

;; Override Marginalia's default
(defun marginalia--file-owner (attrs) ; Only display UID
  "Return file owner given ATTRS."
  (let ((uid (file-attribute-user-id attrs)))
    (when (/= (user-uid) uid)
      (or (user-login-name uid) uid))))

;; TODO: document what the following does
(pcase-dolist (`(,regexp . ,category)
	       '(("\\burl\\b" . url)
		 ("\\bHistory\\b" . history)
		 ("\\bdefinitions\\b" . xref-location)
		 ("\\bxref\\b" . xref-location)))
  (setf (alist-get regexp marginalia-prompt-categories
		   nil nil #'equal)
	category))
(advice-add 'marginalia--buffer-file :filter-return
	    (lambda (buffer-file)
	      (string-trim-left buffer-file "(compilation\\(<.+>\\)? run) ")))

(provide 'marginalia-extras)
;;; marginalia-extras.el ends here
