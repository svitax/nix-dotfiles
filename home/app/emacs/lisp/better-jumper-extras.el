;;; better-jumper-extras.el -*- lexical-binding: t; -*-

(defun doom-set-jump-a (fn &rest args)
  "Set a jump point and ensure fn doesn't set any new jump points."
  (better-jumper-set-jump (if (markerp (car args)) (car args)))
  (let ((evil--jumps-jumping t)
        (better-jumper--jumping t))
    (apply fn args)))

(defun doom-set-jump-maybe-a (fn &rest args)
  "Set a jump point if fn actually moves the point."
  (let ((origin (point-marker))
        (result
         (let* ((evil--jumps-jumping t)
                (better-jumper--jumping t))
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

(defun doom-set-jump-h ()
  "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
  (better-jumper-set-jump)
  nil)

;; Creates a jump point before killing a buffer. This allows you to undo
;; killing a buffer easily (only works with file buffers though; it's not
;; possible to resurrect special buffers).
;;
;; I'm not advising `kill-buffer' because I only want this to affect
;; interactively killed buffers.
(advice-add #'kill-current-buffer :around #'doom-set-jump-a)
(advice-add #'my/kill-this-buffer :around #'doom-set-jump-a)

;; Create a jump point before jumping with imenu.
(advice-add #'imenu :around #'doom-set-jump-a)
(advice-add #'consult-imenu :around #'doom-set-jump-a)

(advice-add #'consult-line :around #'doom-set-jump-a)
(advice-add #'project-find-file :around #'doom-set-jump-a)
(advice-add #'xref-push-marker-stack :around #'doom-set-jump-a)

(provide 'better-jumper-extras)
