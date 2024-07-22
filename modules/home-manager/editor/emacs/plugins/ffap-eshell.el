;;; ffap-eshell.el --- Extras for ffap -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ffap)
(require 'eshell)

(defun guess-directory-from-face ()
  (save-excursion
    (backward-paragraph)
    (forward-char)
    (when (eq (get-text-property (point) 'face) 'eshell-ls-directory)
      (buffer-substring-no-properties
       (point) (next-single-property-change (point) 'face)))))

(defun guess-directory-from-prompt ()
  (save-excursion
    (eshell-previous-prompt 1)
    (end-of-line)
    (thing-at-point 'filename)))

(defun ffap-eshell-mode (name)
  (seq-find #'file-exists-p
            (mapcar (lambda (dir) (expand-file-name name dir))
                    (delq nil (list default-directory
                                    (guess-directory-from-face)
                                    (guess-directory-from-prompt))))))

(setf (alist-get 'eshell-mode ffap-alist) #'ffap-eshell-mode)

;; TODO: can i replace the following functionality with embark?

(defmacro +eshell-ffap (name doc &rest body)
  "Make `find-file-at-point' commands for Eshell.
NAME is how the function is called. DOC is the function's
documentation string. BODY is the set of arguments passed to the
`if' statement to be evaluated when a file at point is present."
  `(defun ,name ()
     ,doc
     (interactive)
     (if-let ((file (ffap-file-at-point)))
	 ,@body
       (user-error "No file at point"))))

;; i'm actually using my own hat here instead.
(+eshell-ffap
 +eshell-ffap-insert
 "Insert (cat) contents of file at point."
 (progn
   (goto-char (point-max))
   (insert (format "hat %s" file))
   (eshell-send-input)))

(+eshell-ffap
 +eshell-ffap-kill-ring-save-path
 "Add to kill-ring the absolute path of file at point."
 (progn
   (kill-new (format "%s/%s" (eshell/pwd) file))
   (message "Copied full path of %s" file)))

(+eshell-ffap
 +eshell-ffap-find-file
 "Run `find-file' for file at point (ordinary file or dir).
Recall that this will produce a `dired' buffer if the file is a
directory."
 (find-file file))

(+eshell-ffap
 +eshell-ffap-dired-jump
 "Jump to the parent directory of the file at point."
 (dired (file-name-directory file)))

(provide 'ffap-eshell)
;;; ffap-eshell.el ends here
