;;; windows-extras.el --- . -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun +scroll-down-half-page ()
  "Scroll down half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
	(lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) (move-to-window-line nil))
	  ((= ln lmax) (recenter (window-end)))
	  (t (progn
	       (move-to-window-line -1)
	       (recenter))))))

(defun +scroll-up-half-page ()
  "Scroll up half a page while keeping the cursor centered."
  (interactive)
  (let ((ln (line-number-at-pos (point)))
	(lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) nil)
	  ((= ln lmax) (move-to-window-line nil))
	  (t (progn
	       (move-to-window-line 0)
	       (recenter))))))

(provide 'windows-extras)
;;; windows-extras.el ends here
