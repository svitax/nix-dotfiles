;;; setup-dangerous-ctl-keys.el -*- lexical-binding: t -*-

(defun my/setup-dangerous-ctl-keys (frame)
  (with-selected-frame frame
    (when (display-graphic-p) ; don't remove this condition if you want terminal Emacs to be usable
      ;; Separate Tab and Return from Ctrl-i, Ctrl-m, and Ctrl-[ by translating them to H-i,
      ;; H-m, and H-m
      (define-key input-decode-map (kbd "C-i") (kbd "H-i"))
      (define-key input-decode-map (kbd "C-m") (kbd "H-m"))
      (define-key input-decode-map (kbd "C-[") (kbd "H-["))
      ;; You can replace `H-' above with anything, it doesn't matter.
      ;; H- is merely a symbol / name; feel free to change it to whatever you
      ;; like.
      )))

(provide 'setup-dangerous-ctl-keys)
;;; setup-dangerous-ctl-keys.el ends here
