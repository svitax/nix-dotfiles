;;; window-extras.el -*- lexical-binding: t; -*-

(defun my/delete-window-or-delete-frame (&optional window)
  "Delete WINDOW using `delete-window'.
If this is the sole window run `delete-frame' instead. WINDOW
must be a valid window and defaults to the selected one. Return
nil."
  (interactive)
  (condition-case nil
      (delete-window window)
    (error (if (and tab-bar-mode
                    (> (length (funcall tab-bar-tabs-function)) 1))
               (tab-bar-cose-tab)
             (delete-frame)))))

(defun my/kill-this-buffer (&optional arg)
  (interactive "P")
  (pcase arg
    ('4 (call-interactively #'kill-buffer))
    (_ (kill-buffer (current-buffer)))))

(defun my/split-window-below (&optional size)
  "Split the selected window into two side-by-side windows.
The selected window is below. The new split-off window is
below and displays the same buffer. Return the new window."
  (interactive "P")
  (select-window
   (if size
       (split-window (frame-root-window)
                     (floor (frame-height) 2)
                     nil nil)
     (split-window-below size)))
  (when (interactive-p)
    (if (featurep 'consult)
        (consult-buffer)
      (call-interactively #'switch-to-buffer))))

(defun my/split-window-right (&optional size)
  "Split the selected window into two side-by-side windows.
The selected window is on the left. The new split-off window
is on the right and displays the same buffer. Return the new
window."
  (interactive "P")
  (select-window
   (if size
       (split-window (frame-root-window)
                     (floor (frame-height) 2)
                     t nil)
     (split-window-right size)))
  (when (interactive-p)
    (if (featurep 'consult)
        (consult-buffer)
      (call-interactively #'switch-to-buffer))))

(defun my/scroll-down (arg)
  "Move cursor down half a screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (forward-line dist)))

(defun my/scroll-up (arg)
  "Move cursor up half a screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 2)))
    (forward-line (- dist))))

(defun my/toggle-window-split ()
  "Toggle window split from vertical to horizontal."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows.")
    (let ((was-full-height (window-full-height-p)))
      (delete-other-windows)
      (if was-full-height
          (split-window-vertically)
        (split-window-horizontally))
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(defun my/transpose-windows ()
  "Swap the buffers shown in current and next window."
  (interactive)
  (let ((this-buffer (window-buffer))
        (next-window (next-window nil :no-minibuf nil)))
    (set-window-buffer nil (window-buffer next-window))
    (set-window-buffer next-window this-buffer)
    (select-window next-window)))

(provide 'window-extras)
