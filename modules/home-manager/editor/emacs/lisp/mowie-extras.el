;;; mowie-extras.el --- . -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +beginning-of-line ()
  (interactive "^")
  (mowie
   #'beginning-of-line
   #'beginning-of-visual-line
   #'mowie-beginning-of-code
   #'mowie-beginning-of-comment
   #'mowie-beginning-of-comment-text))

(defun +end-of-line ()
  (interactive "^")
  (mowie
   #'end-of-line
   #'end-of-visual-line
   #'mowie-end-of-code))

(provide 'mowie-extras)
;;; mowie-extras.el ends here
