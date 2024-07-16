;;; better-jumper-extras.el -*- lexical-binding: t -*-

(defun my/set-jump-maybe-a (fn &rest args)
  "Set a jump point if fn actually moves the point."
  (let ((origin (point-marker))
	(result
	 (let* ((better-jumper--jumping t))
	   (apply fn args)))
	(dest (point-marker)))
    (unless (equal origin dest)
      (with-current-buffer (marker-buffer origin)
	(better-jumper-set-jump
	 (if (markerp (car args))
	     (car args)
	   origin))))
    (set-marker origin nil)
    (set-marker dest nil)
    result))

(defun my/set-jump-h (&rest args)
  "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
  (better-jumper-set-jump)
  nil)

(provide 'better-jumper-extras)
;;; better-jumper-extras.el ends here
