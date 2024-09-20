;;; meow-macrursors.el -*- lexical-binding: t; -*-

(require 'meow)
(require 'macrursors)

;; meow-macrursors workflow:

;; 1 - `C-\;' to enter `meow-macrursors-mode'
;; in macrursors-state, move normally by inheriting from parent 'meow-normal-mode'
;; x, w, W, etc. use 'macrursors-mark-all-*'
;; . to 'macrursors-mark-all-instances-of'
;; C-n to 'macrursors-mark-next-line'
;; C-e to 'macrursors-mark-previous-line'
;; C-SPC to 'macrursors-select'
;; when defining a kmacro, don't let <ESC> quit recording the kmacro
;; <esc> to noop, or early-quit
;; C-; to 'macrursors-end'
;; 
;; '("," . +meow-keep-primary-selection) ;; NOTE: (helix)
;; '("M-," . +meow-remove-primary-selection) ;; NOTE: (helix)

;; Two ways to use Beacon:
;; 1 - Switch to `meow-insert-mode' and start recording kmacros with `meow-macrursors-insert', `meow-macrursors-append', `meow-macrursors-change', finish recording, and apply this kmacro to all cursors/regions by exiting meow-insert-mode with `meow-macrursors-insert-exit'
;; 2 - Start recording with `kmacro-start-macro', finish recording and apply this kmacro to all cursors/regions with `kmacro-end-macro'.

;; C-; enters `meow-macrursors-mode'

;; `meow-macrursors-mode'
;; "l" #'meow-macrursors-insert  ;; (calls `macrursors-start')
;; "a" #'meow-macrursors-append  ;; (calls `macrursors-start')
;; "c" #'meow-macrursors-change  ;; (calls `macrursors-start')
;; "r" #'meow-macrursors-replace ;; (calls `macrursors-start')
;; "d" #'meow-macrursors-delete  ;; calls `macrursors-start'. deletes all selections, but only real selection is saved to kill-ring
;; "x" #'meow-macrursors-lines
;; "w" #'meow-macrursors-words
;; "W" #'meow-macrursors-symbols
;; "s" #'meow-macrursors-select ;; like isearch but exits with active selection on search query (is this possible with emacs?)
;; "C-s" #'meow-macrursors-isearch-forward
;; "C-r" #'meow-macrursors-isearch-backward
;; "," #'meow-macrursors-remove-cursors

;; "C-g" #'meow-macrursors-early-quit

;; `meow-insert-mode'
;; "<esc> "#'meow-macrursors-insert-exit ;; (injects `macrursors-end' somehow)

(setq meow-macrursors-keymap (make-keymap))

(meow-define-state macrursors
  "Meow state for macrursors."
  :lighter " [M]"
  :keymap meow-macrursors-keymap)

(setq meow-cursor-type-macrursors 'hollow)

(meow-define-keys 'macrursors
  ;; TODO: inherit from meow-normal-mode
  '("l" . meow-macrursors-insert)
  '("a" . meow-macrursors-append)
  '("c" . meow-macrursors-change)
  '("r" . meow-macrursors-replace)
  '("d" . meow-macrursors-delete)
  '("C-g" . macrursors-early-quit))

;; BUG: current roadblocks
;; `macrursors-mark-next-instance-of' on an active selection + `meow-append' includes the 'a' in the kmacro
;; `macrursors-mark-next-instance-of' + `meow-append' doesn't include the 'a' in the kmacro
;; NOTE: solution is to cancel region selection whenever macrursors-mode is enabled

(provide 'meow-macrursors)
;;; meow-macrursors.el ends here
