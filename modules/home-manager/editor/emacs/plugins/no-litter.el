;;; no-litter.el -*- lexical-binding: t; -*-

(defvar user-cache-directory "~/.cache/emacs/"
  "Location where files created by Emacs are placed.")

(defvar user-etc-directory
  (file-name-concat user-cache-directory "etc/")
  "The directory where packages place their configuration files.")

(defvar user-var-directory
  (file-name-concat user-cache-directory "var/")
  "The directory where packages place their persistent data files.")

(defun expand-etc-file-name (file)
  "Expand filename FILE relative to `user-etc-directory'."
  (file-name-concat user-etc-directory
		    (convert-standard-filename file)))

(defun expand-var-file-name (file)
  "Expand filename FILE relative to `user-var-directory'."
  (file-name-concat user-var-directory
		    (convert-standard-filename file)))

(defalias 'etc #'expand-etc-file-name)
(defalias 'var #'expand-var-file-name)

(provide 'no-litter)
;;; no-litter.el ends here
