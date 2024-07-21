;;; files-extras.el -*- lexical-binding: t; -*-

(defun +files-create-non-existent-directory ()
  "Create a non-existent directory."
  (when-let* ((file-name buffer-file-name)
	      (parent-directory (file-name-directory file-name)))
    (when (and (not (file-exists-p parent-directory))
	       (y-or-n-p (format "Create `%s' dis? " parent-directory)))
      (make-directory parent-directory t))))

(provide 'files-extras)
;;; files-extras.el ends here
