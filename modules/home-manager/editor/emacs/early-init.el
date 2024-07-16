(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

(defun ev/disable-scroll-bars (frame)
  "Remove all possible scroll-bars, both vertical and horizontal."
  (modify-frame-parameters frame
			   '((vertical-scroll-bars . nil)
			     (horizontal-scroll-bars . nil))))

(add-hook 'after-make-frame-functions 'ev/disable-scroll-bars)
