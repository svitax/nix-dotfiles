;;; meow-extras.el -*- lexical-binding: t; -*-

;; Setup prefix maps

(setq +bibliography-prefix-map (make-sparse-keymap))
(setq +buffer-prefix-map (make-sparse-keymap))
(setq +compile-prefix-map (make-sparse-keymap))
(setq +eval-prefix-map (make-sparse-keymap))
(setq +file-prefix-map (make-sparse-keymap))
(define-prefix-command '+goto-prefix-map)
(define-prefix-command '+match-prefix-map)
(setq +notes-prefix-map (make-sparse-keymap))
(setq +project-prefix-map (make-sparse-keymap))
(setq +quit-prefix-map (make-sparse-keymap))
(setq +search-prefix-map (make-sparse-keymap))
(setq +toggle-prefix-map (make-sparse-keymap))
(setq +vc-prefix-map (make-sparse-keymap))
(setq +window-prefix-map (make-sparse-keymap))

(defmacro +meow--call-negative (form)
  `(let ((current-prefix-arg -1))
     (call-interactively ,form)))

(defun +meow-find-backwards ()
  (interactive)
  (+meow--call-negative 'meow-find))

(defun +meow-till-backwards ()
  (interactive)
  (+meow--call-negative 'meow-till))

(defun +meow-append-line-end ()
  "Move to the end of the current line, switch to INSERT mode."
  (interactive)
  (meow-line 1)
  (meow-append))

(defun +meow-insert-line-start ()
  "Move to the start of the current line, switch to INSERT mode."
  (interactive)
  (meow-join t)
  (meow-append))

(defun +meow-join-line ()
  (interactive)
  (meow-join -1)
  (meow-kill))

(defun +meow-lookup ()
  "grab from lambda emacs or something"
  (interactive)
  (eldoc-doc-buffer))

(defun +replace-char (arg char)
  "Replace current char."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char-from-minibuffer "char: "
						nil 'read-char-history)))
  (progn
    (call-interactively 'delete-char)
    (insert-char char)))

(defun +meow-replace ()
  "Replace region with yank if active. Otherwise replace char at point."
  (interactive)
  (if (region-active-p)
      (call-interactively 'meow-replace)
    (call-interactively '+replace-char)))

(defun +meow-duplicate ()
  "Duplicate region if active. Otherwise duplicate char at point."
  (interactive)
  (if (region-active-p)
      (progn
	(meow-save)
	(meow-yank))
    (progn
      (meow-save-char)
      (meow-yank))))

;; TODO: +meow-kill
(defun +meow-kill ()
  (interactive)
  (if (region-active-p)
      (meow-kill)
    (meow-delete)))

;; TODO: change meow-simple-motion-keymap to meow-simple-motion-state-keymap
(setq meow-simple-motion-keymap (make-keymap))
(meow-define-state simple-motion
  "Meow state for motion without meow commands."
  :lighter " [m]"
  :keymap meow-simple-motion-keymap)
(meow-define-keys 'simple-motion
  '("<escape>" . meow-motion-mode)
  ;; '("n" . "n")
  '("e" . "p")
  '("SPC" . meow-keypad))

;; TODO: finish meow-expand-mode
;; TODO: change meow-expand-keymap to meow-expand-state-keymap
;; TODO: make meow-expand-mode inherit from meow-normal-mode
(setq meow-expand-keymap (make-keymap))
(set-keymap-parent meow-expand-keymap meow-normal-state-keymap)
(meow-define-state expand
  "Meow state for expanding selections."
  :lighter " [E]"
  :keymap meow-expand-keymap)
(meow-define-keys 'expand
  '("<escape>" . meow-normal-mode)
  '("l" . meow-insert)
  '("a" . meow-append)
  '("y" . meow-save)
  '("m" . meow-left-expand)
  '("n" . meow-next-expand)
  '("e" . meow-prev-expand)
  '("i" . meow-right-expand))

;; TODO: finish meow-structural-mode
(setq meow-structural-keymap (make-keymap))
(meow-define-state structural
  "Meow state for structured editing and navigation."
  :lighter " [S]"
  :keymap meow-structural-keymap)
(setq meow-cursor-type-structural 'hollow)
(meow-define-keys 'structural
  '("<escape>" . meow-normal-mode)
  '("u" . meow-undo)
  ;; slurp
  ;; barf
  ;; raise
  ;; absorb
  ;; split
  ;; transpose
  '("f" . sp-forward-sexp)
  '("b" . sp-backward-sexp)
  '("m" . sp-beginning-of-sexp)
  '("i" . sp-end-of-sexp)
  '("n" . sp-down-sexp)
  '("e" . sp-up-sexp))

(provide 'meow-extras)
;;; meow-extras.el ends here
