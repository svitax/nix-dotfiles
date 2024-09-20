;;; themes-extras.el -*- lexical-binding: t; -*-

(defun my/ef-themes-custom-faces (&rest _)
  "My customization on top of `ef-themes'.
This function is added to `ef-themes-post-load-hook'."
  (ef-themes-with-colors
   (custom-set-faces
    `(diff-hl-change ((,c :foreground ,bg-changed-refine :background ,bg-main)))
    `(diff-hl-delete ((,c :foreground ,bg-removed-refine :background ,bg-main)))
    `(diff-hl-insert ((,c :foreground ,bg-added-refine :background ,bg-main))))))

;; Using the hook lets our changes persist when we use the command
;; `modus-themes-toggle', `modus-themes-select', and `modus-themes-load-random'.

(defun my/modus-themes-custom-faces (&rest _)
  "My customization on top of `modus-themes'.
This function is added to `modus-themes-post-load-hook'."
  (modus-themes-with-colors
    (custom-set-faces
     `(org-ellipsis ((,c :inherit 'default :box nil)))
     `(git-gutter-fr:added ((,c :foreground ,bg-added-fringe :background ,fringe)))
     `(git-gutter-fr:deleted ((,c :foreground ,bg-removed-fringe :background ,fringe)))
     `(git-gutter-fr:modified ((,c :foreground ,bg-changed-fringe :background ,fringe)))
     `(avy-lead-face ((,c :foreground ,fg-main :background ,bg-magenta-subtle :weight bold)))
     `(avy-lead-face-0 ((,c :background ,bg-cyan-subtle)))
     `(avy-lead-face-1 ((,c :background ,bg-green-subtle)))
     `(avy-lead-face-2 ((,c :background ,bg-red-subtle)))
     `(avy-goto-char-timer-face ((,c :foreground ,fg-main :background ,bg-yellow-intense)))
     `(easy-kill-selection ((,c :inherit modus-themes-mark-sel)))
     `(diff-hl-change ((,c :foreground ,bg-changed-fringe :background ,fringe)))
     `(diff-hl-delete ((,c :foreground ,bg-removed-fringe :background ,fringe)))
     `(diff-hl-insert ((,c :foreground ,bg-added-fringe :background ,fringe))))))

;; Using the hook lets our changes persist when we use the command
;; `ef-themes-toggle', `ef-themes-select', and `ef-themes-load-random'.

(provide 'themes-extras)
;;; themes-extras.el ends here
