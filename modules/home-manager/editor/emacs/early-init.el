;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether it has a noticeable effect on Linux and Mac hasn't
;; been determined, but we inhibit it there anyway.
(setq inhibit-compacting-font-caches t)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1.0)

;; Don't want a modeline while loading init.
(setq mode-line-format nil)

;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No menu-bar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; No tool-bar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; No tooltip by default.
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;; No alarms by default
(setq ring-bell-function 'ignore)

(defun my/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (let* ((buffer-read-only)
	   (image-path "~/.config/emacs/emacs.png")
	   (image (create-image image-path))
	   (size (image-size image))
	   (height (cdr size))
	   (width (car size))
	   (top-margin (floor (/ (- (window-height) height) 2)))
	   (left-margin (floor (/ (- (window-width) width) 2)))
	   (title "Welcome to Emacs!"))
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n ))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      (insert (make-string (floor (/ (- (window-width) (string-width title)) 2)) ?\ ))
      (insert title))
    (setq cursor-type nil)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "q") 'kill-this-buffer)))

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

(when (< (length command-line-args) 2)
  ;; TODO: maybe i should add to `emacs-startup-hook' as well in case i'm not using emacsclient
  ;; FIXME: this gets called after i finish commiting in magit
  (add-hook 'server-after-make-frame-hook (lambda ()
					    (when (display-graphic-p)
					      (my/show-welcome-buffer)))))

(push '(internal-border-width . 16) default-frame-alist)
(push '(right-divider-width . 1) default-frame-alist)
(push '(scroll-bar-width . 8) default-frame-alist)
