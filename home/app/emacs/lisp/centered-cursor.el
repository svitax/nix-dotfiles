;;; centered-cursor.el -*- lexical-binding: t; -*-

(defun my/disable-centered-cursor ()
  (setq-local scroll-preserve-screen-position nil
	      maximum-scroll-margin 0.25
	      scroll-margin 0))

(setq scroll-preserve-screen-position t
      scroll-conservatively 0
      maximum-scroll-margin 0.5
      scroll-margin 99999)

(provide 'centered-cursor)
