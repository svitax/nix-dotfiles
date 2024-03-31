;;; recentf-extras.el -*- lexical-binding: t; -*-

;; Keep track of recently opened files. Also feeds into the list of recent
;; directories used by consult-dir.
(defun doom--recentf-file-truename-fn (file)
  (if (or (not (file-remote-p file))
          (equal "sudo" (file-remote-p file 'method)))
      (abbreviate-file-name (file-truename (tramp-file-name-localname file)))
    file))

;; Exclude anything in runtime folders
(add-to-list 'recentf-exclude
             (concat "^" (regexp-quote (or (getenv "XDG_RUNTIME_DIR")
                                           "/run"))))
;; Exclude anything in nix store
(add-to-list 'recentf-exclude (concat "^" (regexp-quote "/nix/store")))

;; Resolve symlinks, strip out the /sudo:X@ prefix in local tramp paths, and
;; abbreviate $HOME -> ~ in filepaths (more portable, more reaadable, & saves
;; space)
(add-to-list 'recentf-filename-handlers #'doom--recentf-file-truename-fn)
;; Text properties inflate the size of recentf's files, and there is no
;; purpose in persisting them (Must be first in the list!)
(add-to-list 'recentf-filename-handlers #'substring-no-properties)

(defun doom--recentf-touch-buffer-h ()
  "Bump file in recent file list when it is switched or written to."
  (when buffer-file-name
    (recentf-add-file buffer-file-name))
  ;; Return nil for `write-file-functions'
  nil)

(add-hook 'on-switch-window-hook 'doom--recentf-touch-buffer-h)
(add-hook 'write-file-functions 'doom--recentf-touch-buffer-h)

(defun doom--recentf-add-dired-directory-h ()
  "Add Dired directories to recentf file list."
  (recentf-add-file default-directory))

(add-hook 'dired-mode-hook 'doom--recentf-add-dired-directory-h)

;; The most sensible time to clean up your recent files list is when you quit
;; Emacs (unless this is a long-running daemon session).
(setq recent-auto-cleanup (if (daemonp) 300))
(add-hook 'kill-emacs-hook #'recentf-cleanup)

(provide 'recentf-extras)
