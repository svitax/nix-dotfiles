;;; theme.el --- Emacs theme and related setup -*- lexical-binding: t; -*-

(defun modus-themes-custom-faces ()
  "My customization on top of the Modus themes.
This function is added to the `modus-themes-post-load-hook'."
  (modus-themes-with-colors
    (custom-set-faces
     `(eshell-prompt ((,c :foreground ,blue)))
     `(git-gutter-fr:added ((,c :foreground ,green :background ,green)))
     `(git-gutter-fr:modified ((,c :foreground ,yellow :background ,yellow)))
     `(diff-hl-insert ((,c :foreground ,green :background ,green)))
     `(diff-hl-change ((,c :foreground ,yellow :background ,yellow)))
     `(diff-hl-delete ((,c :foreground ,red :background ,red))))))

(defun ef-themes-custom-faces (&rest _args)
  "My customizations on top of the Ef themes.
This function is added to the `ef-themes-post-load-hook'."
  (ef-themes-with-colors
    (custom-set-faces
     `(pulsar-blue ((,c :background ,bg-blue-subtle)))
     `(pulsar-cyan ((,c :background ,bg-cyan-subtle)))
     `(pulsar-green ((,c :background ,bg-green-subtle)))
     `(pulsar-magenta ((,c :background ,bg-magenta-subtle)))
     `(pulsar-red ((,c :background ,bg-red-subtle)))
     `(pulsar-yellow ((,c :background ,bg-yellow-subtle)))
     ;; Make the mode line like the `modus-themes' default
     `(mode-line ((,c :background ,bg-active :foreground ,fg-main :box (:line-width 1 :color ,fg-dim))))
     `(mode-line-inactive ((,c :box (:line-width 1 :color ,bg-active))))
     `(git-gutter-fr:added ((,c :foreground ,green :background ,green)))
     `(git-gutter-fr:modified ((,c :foreground ,yellow :background ,yellow)))
     `(diff-hl-insert ((,c :foreground ,green :background ,green)))
     `(diff-hl-change ((,c :foreground ,yellow :background ,yellow)))
     `(diff-hl-delete ((,c :foreground ,red :background ,red)))
     `(avy-lead-face ((,c :background ,yellow)))
     `(highlight-quoted-symbol ((,c :foreground ,blue))))))

;; I want to use circles as my flymake fringe bitmap
(define-fringe-bitmap 'dot-fringe-bitmap
  (vector #b00000000
		  #b00000000
		  #b00000000
		  #b00000000
		  #b00000000
		  #b00000000
		  #b00000000
		  #b00011100
		  #b00111110
		  #b00111110
		  #b00111110
		  #b00011100
		  #b00000000
		  #b00000000
		  #b00000000
		  #b00000000
		  #b00000000))

;; Generally user options should never be touched by a theme. However, according
;; to the maintainer of modus-themes, certain cases like `hl-todo-keyword-faces'
;; and the `flymake-*-bitmap' variants merit an exception.
;; This is annoying because I don't like the face used for Flymake bitmaps.
;; I would like them to not have a background color. These variables need to be
;; set after loading the theme.
(defun my/modus-themes-flymake-bitmaps ()
  "Override Flymake bitmaps to blend into the fringe"
  (customize-set-variable
   'flymake-error-bitmap '(dot-fringe-bitmap compilation-error))
  (customize-set-variable
   'flymake-warning-bitmap '(dot-fringe-bitmap compilation-warning))
  (customize-set-variable
   'flymake-note-bitmap '(dot-fringe-bitmap compilation-info)))

(provide 'theme)
