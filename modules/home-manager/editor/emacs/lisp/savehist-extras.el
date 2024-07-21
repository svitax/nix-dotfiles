;;; savehist-extras.el -*- lexical-binding: t; -*-

(defun +savehist-unpropertize-variables-h ()
  "Remove text properties from `kill-ring' to reduce savehist cache size."
  (setq kill-ring
	(mapcar #'substring-no-properties
		(cl-remove-if-not #'stringp kill-ring))
	register-alist
	(cl-loop for (reg . item) in register-alist
		 if (stringp item)
		 collect (cons reg (substring-no-properties item))
		 else collect (cons reg item))))

(defun +savehist-remove-unprintable-registers-h ()
  "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwriteable tidbits."
  ;; Save new value in the temp buffer savehist is running
  ;; `savehist-save-hook' in. We don't want to actually remove the
  ;; unserializable registers in the current session!
  (setq-local register-alist
	      (cl-remove-if-not #'savehist-printable register-alist)))

(provide 'savehist-extras)
;;; savehist-extras.el ends here
