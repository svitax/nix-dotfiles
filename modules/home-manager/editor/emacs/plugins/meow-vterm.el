;;; meow-vterm.el --- Meow Vterm support -*- lexical-binding: t -*-

;; Author: 45mg
;; Maintainer: 45mg <45mg@no.mail>
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") meow vterm)
;; Homepage: TODO

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This package allows for the use of a subset of Meow's features in `vterm-mode'.
;;
;; Vterm does not support standard editing and movement over the input text. It
;; instead passes each keystroke to a terminal emulator (Libvterm), and updates
;; the display to reflect the updated cursor position and text. This means that
;; standard Meow commands cannot work.
;;
;; `meow-vterm' works around this by enabling `vterm-copy-mode' along with
;; Normal state, which pauses display updates and makes the buffer Read-Only.
;; Commands that change the text are implemented by temporarily disabling Copy
;; Mode, performing the change, and enabling it again.
;;
;; Setup:
;;
;;    (add-hook 'vterm-mode-hook #'meow-vterm-mode)
;;    (meow-define-keys 'vterm-normal
;;      '("p" . meow-vterm-yank)
;;      '("P" . meow-vterm-yank-pop)
;;      '("u" . meow-vterm-undo)
;;      '("p" . meow-vterm-yank)
;;      '("P" . meow-vterm-yank-pop)
;;      '("s" . meow-vterm-kill)
;;      '("d" . meow-vterm-delete)
;;      '("D" . meow-vterm-backspace)
;;      '("G" . ignore)) ; see below
;;
;; Limitations:
;;
;; The secondary selection tends to not work properly.
;; When Copy Mode is disabled, the region and secondary selection vanish, and it
;; is unreliable to save and restore the region and secondary selection
;; positions because Vterm inserts and removes 'fake newlines' from time to
;; time, making markers unreliable. `meow-sync-grab' and `meow-swap-grab' work
;; 90% of the time, but Beacon state is out of the question, and it's best to
;; avoid using the secondary selection entirely.
;;
;; `meow-join' is not supported. Vterm only lets you type one line at a time
;; anyway.

;;; Code:

(require 'meow)
(require 'vterm)

(defvar meow-vterm--kbd-undo "C-x u"
  "KBD macro for `undo' in Vterm.
By default, Vterm maps \\='C-/\\=' and \\='C-_\\=' to `vterm-undo', which just
sends \\='C-_\\=' to your shell. The default value of this variable will make
Meow invoke `undo' instead (unless you've changed the default binding).")

(defun meow-vterm--allow-modify-a ()
  "HACK: :before-until advice for `meow--allow-modify-p' to make it always
succeed in Vterm Normal state."
  (bound-and-true-p meow-vterm-normal-mode))

(defun meow-vterm--insert-redirect ()
  "Add to `meow-insert-mode-hook' buffer-locally to make functions enter Vterm
Insert state instead of regular Insert state."
  (and meow-insert-mode (meow--switch-state 'vterm-insert)))

(defun meow-vterm--normal-redirect ()
  "Add to `meow-normal-mode-hook' buffer-locally to make functions enter Vterm
Normal state instead of regular Normal state."
  (and meow-normal-mode (meow--switch-state 'vterm-normal)))

(defmacro meow-vterm--without-copy-mode (&rest body)
  "Disable Vterm copy mode, execute BODY, then enable it again.
Preserve mark and secondary selection."
  (declare (indent 0) (debug t))
  `(let ((m (mark))
         (p (point))
         (ss (overlay-start mouse-secondary-overlay))
         (se (overlay-end mouse-secondary-overlay)))
     (unwind-protect
         (progn
           (vterm-copy-mode -1)
           (vterm-goto-char p)
           ,@body)
       (vterm-copy-mode +1)
       (when (and ss se)
         (save-mark-and-excursion
           (set-mark ss)
           (goto-char se)
           (secondary-selection-from-region)))
       (and m (set-mark m)))))

(defun meow-vterm--delete-region-function (start end)
  "Wrapped `vterm-delete-region' that works in Vterm copy mode."
  (meow-vterm--without-copy-mode
    ;; BUG `vterm-delete-region' does nothing and returns nil if start>end;
    ;; it should be able to handle this condition, like `delete-region'
    (vterm-delete-region (min start end) (max start end))))

(defun meow-vterm--insert-function (&rest args)
  "Wrapped `vterm-insert' that works in Vterm copy mode."
  (meow-vterm--without-copy-mode (apply #'vterm-insert args)))

(defun meow-vterm-yank ()
  "Wrapped `meow-yank' that works in Vterm copy mode."
  (interactive)
  (meow-vterm--without-copy-mode (call-interactively #'meow-yank)))

(defun meow-vterm-yank-pop ()
  "Wrapped `meow-yank-pop' that works in Vterm copy mode."
  (interactive)
  (meow-vterm--without-copy-mode (call-interactively #'meow-yank-pop)))

(defun meow-vterm-undo ()
  "Wrapped `meow-undo' that works in Vterm copy mode.
As Vterm by default sends C-/ or C-_ as C-_ to the shell, you may want to
`setq-local' `meow--kbd-undo' so that this command invokes `undo'."
  (interactive)
  (meow-vterm--without-copy-mode (call-interactively #'meow-undo)))

(defun meow-vterm-kill ()
  "Kill region, or till end of line."
  (interactive)
  (meow--direction-backward)
  (let ((p (point))
        (m (if (region-active-p) (mark) (line-end-position))))
    (kill-ring-save p m 'region)
    (meow-vterm--delete-region-function p m)))

(defun meow-vterm-delete ()
  "Wrapped `meow-delete' that works in Vterm copy mode."
  (interactive)
  (meow-vterm--without-copy-mode
    (when vterm--term
      (call-interactively #'vterm-send-delete)
      (vterm--update vterm--term)
      (setq vterm--redraw-immididately t)
      (accept-process-output vterm--process vterm-timer-delay nil t))))

(defun meow-vterm-backspace ()
  "Wrapped `meow-backspace' that works in Vterm copy mode."
  (interactive)
  (meow-vterm--without-copy-mode
    (when vterm--term
      (call-interactively #'vterm-send-backspace)
      (vterm--update vterm--term)
      (setq vterm--redraw-immididately t)
      (accept-process-output vterm--process vterm-timer-delay nil t))))

;;;###autoload
(define-minor-mode meow-vterm-mode
  "Meow minor mode for Vterm buffers."
  :lighter " Meow-Vterm"
  (let ((state (alist-get 'vterm-mode meow-mode-state-list)))
    (if meow-vterm-mode
        (progn
          (meow--switch-state
           (if (memq state '(vterm-insert insert)) 'vterm-insert 'vterm-normal))
          (add-hook 'meow-insert-mode-hook #'meow-vterm--insert-redirect 0 t)
          (add-hook 'meow-normal-mode-hook #'meow-vterm--normal-redirect 0 t)
          (setq-local meow--delete-region-function
                      #'meow-vterm--delete-region-function)
          (setq-local meow--insert-function
                      #'meow-vterm--insert-function)
          (setq-local meow--kbd-undo meow-vterm--kbd-undo)
          (advice-add #'meow--allow-modify-p
                      :before-until #'meow-vterm--allow-modify-a)
          (advice-add #'meow-normal-mode-p
                      :before-until #'meow-vterm-normal-mode-p)
          (advice-add #'meow-insert-mode-p
                      :before-until #'meow-vterm-insert-mode-p))
      (meow--switch-state state)
      (remove-hook 'meow-insert-mode-hook #'meow-vterm--insert-redirect t)
      (remove-hook 'meow-normal-mode-hook #'meow-vterm--normal-redirect t)
      (setq-local meow--delete-region-function
                  (default-value 'meow--delete-region-function))
      (setq-local meow--insert-function
                  (default-value 'meow--insert-function))
      (setq-local meow--kbd-undo
                  (default-value 'meow--kbd-undo))
      (advice-remove #'meow--allow-modify-p
                     #'meow-vterm--allow-modify-a)
      (advice-remove #'meow-normal-mode-p
                     #'meow-vterm-normal-mode-p)
      (advice-remove #'meow-insert-mode-p
                     #'meow-vterm-insert-mode-p))))

(defvar meow-vterm-normal-state-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map meow-normal-state-keymap)
    map)
  "Keymap for Vterm Normal state.")

(meow-define-state vterm-normal
  "Variant of Normal state for Vterm."
  :keymap meow-vterm-normal-state-keymap
  :face meow-normal-cursor
  (if meow-vterm-normal-mode
      (progn
        (vterm-copy-mode +1))
    ;; Vterm resets point when exiting copy mode, so we need to work around
    ;; that.
    (unless (meow--beacon-inside-secondary-selection)
      (let ((p (point)))
        (vterm-copy-mode -1)
        (vterm-goto-char p)))))

(setq meow-cursor-type-vterm-normal meow-cursor-type-normal)

(defvar meow-vterm-insert-state-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map meow-insert-state-keymap)
    map)
  "Keymap for Vterm Insert state.")

(meow-define-state vterm-insert
  "Variant of Insert state for Vterm."
  ;; copied from definition of Insert state
  :keymap meow-vterm-insert-state-keymap
  :face meow-insert-cursor
  ;; TODO we should not need the `progn' here; `meow-define-state' should run
  ;; ALL forms after the keyword args, not just the first one. PR a fix
  ;; upstream.
  (progn
    (if meow-vterm-insert-mode
        (run-hooks 'meow-insert-enter-hook)
      (when (and meow--insert-pos
                 meow-select-on-change
                 (not (= (point) meow--insert-pos)))
        (thread-first
          (meow--make-selection '(select . transient) meow--insert-pos (point))
          (meow--select)))
      (run-hooks 'meow-insert-exit-hook)
      (setq-local meow--insert-pos nil))
    (run-hooks 'meow-insert-mode-hook)))

(setq meow-cursor-type-vterm-insert meow-cursor-type-insert)

(provide 'meow-vterm)
;;; meow-vterm.el ends here
