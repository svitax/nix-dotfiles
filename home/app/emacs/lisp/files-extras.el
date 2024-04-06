;;; files-extras.el -*- lexical-binding: t; -*-

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if it doesn't exist while visiting file."
  (unless (file-exists-p filename)
	(let ((dir (file-name-directory filename)))
	  (unless (file-exists-p dir)
		(make-directory dir t)))))

(provide 'files-extras)
