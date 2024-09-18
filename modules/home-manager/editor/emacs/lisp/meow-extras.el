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

(defun +meow-window-top ()
  "Move the cursor to the top line in the window."
  (interactive)
  (move-to-window-line
   (max 0 (if (= (point-min) (window-start)) 0 scroll-margin))))

(defun +meow-window-middle ()
  "Move the cursor to the middle line in the window."
  (interactive)
  (move-to-window-line nil))

(defun +meow-window-bottom ()
  "Move the cursor to the bottom line in the window."
  (interactive)
  (move-to-window-line (- (max 1 (1+ scroll-margin)))))

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

(provide 'meow-extras)
;;; meow-extras.el ends here
