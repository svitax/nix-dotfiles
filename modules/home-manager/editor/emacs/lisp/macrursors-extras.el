;;; macrursors-extras.el -*- lexical-binding: t; -*-

(defun +macrursors-select ()
  "Create a secondary selection from an active region, otherwise clear secondary selection."
  (interactive)
  (cond ((use-region-p)
	 (macrursors-select))
	((overlay-buffer mouse-secondary-overlay)
	 (macrursors-select-clear))))

(defun +macrursors-at-avy ()
  "Create cursors selectively with `avy' and `macrursors.'"
  (interactive)
  (let* ((avy-all-windows nil)
	 (positions (mapcar #'caar (avy--read-candidates))))
    (when positions
      (mapc #'macrursors--add-overlay-at-point positions)
      (macrursors-start))))

(defun +toggle-meow-during-macro ()
  (if (bound-and-true-p macrursors-mode)
      (meow-global-mode -1)
    (meow-global-mode 1)))

(provide 'macrursors-extras)
;;; macrursors-extras.el ends here
