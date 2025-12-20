;;; flymake-golangci.el -- Flymake checker for golangci linter -*- lexical-binding: t -*-

;; Copyright (C) 2024 Petter Storvik

;; Author: Petter Storvik <petterstorvik@gmail.com>
;; Keywords: linter, tools, go
;; URL: https://github.com/storvik/flymake-golangci
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (flymake-collection "2.0"))

;;; Commentary:

;; Flymake backend for golangci-lint linter.
;;
;; Usage:
;;
;; Add backend to go-mode with:
;;
;;   (add-hook 'go-mode #'flymake-golangci-load-backend)
;;
;; Or if using eglot:
;;
;;   (add-hook 'eglot-managed-mode #'flymake-golangci-load-backend)
;;
;; Note, this does not enable flymake or eldoc.
;;

;;; Code:

(require 'flymake)
(require 'flymake-collection)

(eval-when-compile
  (require 'flymake-collection-define))

(defgroup flymake-collection-golangci nil
  "Flymake backend for golangci-lint."
  :prefix "flymake-collection-golangci-"
  :group 'flymake-collection)

;; TODO actually use flymake-collection-golangci-executable and delete any unnecessary groups
;; (defcustom flymake-collection-golangci-executable "golangci-lint"
;;   "Path to golangci-lint executable."
;;   :group 'flymake-collection-golangci
;;   :type 'string)

(defcustom flymake-collection-golangci-args nil
  "Additional command line arguments passed to golangci-lint."
  :type '(repeat string)
  :group 'flymake-collection-golangci)

;;;###autoload (autoload 'flymake-collection-golangci "flymake-collection-golangci")
(flymake-collection-define-enumerate flymake-collection-golangci
  "A Go syntax and style checker using golangci-lint.

See URL `https://golangci-lint.run/'."
  :title "golangci-lint"
  :pre-let ((golangci-exec (executable-find "golangci-lint")))
  :pre-check (unless golangci-exec
               (error "Cannot find golangci-lint executable"))

  :write-type 'pipe
  :command `(,golangci-exec
             "run"
             "--out-format" "json"
             ,@flymake-collection-golangci-args
             ,@(when-let ((file (buffer-file-name flymake-collection-source)))
                 (list file)))
  :generator
  (let ((parsed (json-parse-string
                 (buffer-substring-no-properties (point-min) (point-max))
                 :object-type 'alist
                 :array-type 'list
                 :null-object nil
                 :false-object nil)))
    (alist-get 'Issues parsed))
  :enumerate-parser
  (let-alist it
    (let* ((pos .Pos)
           (row (alist-get 'Line pos))
           (col (alist-get 'Column pos))
           (region (flymake-diag-region flymake-collection-source row col)))
      (list flymake-collection-source
            (car region)
            (cdr region)
            :warning
            (concat (propertize .FromLinter 'face 'flymake-collection-diag-id)
                    ": " .Text)))))

;;;###autoload
(defun flymake-golangci-load-backend ()
  "Loads golangci into `flymake-diagnostic-functions'."
  (add-hook 'flymake-diagnostic-functions 'flymake-collection-golangci nil t))

;;;###autoload
(defun flymake-golangci-lint-project ()
  "Lint entire project with `golangci-lint'."
  (interactive)
  (let ((current-directory (project-current)))
    (call-process flymake-golangci-executable
                  nil
                  (get-buffer-create "*golangci-lint run*")
                  nil
                  "run")))

;;;###autoload
(defun flymake-golangci-clear-cache ()
  "Clear `golangci-lint' cache."
  (interactive)
  (unless (executable-find flymake-golangci-executable)
    (error "Cannot find golangci-lint, is it installed?"))
  (call-process flymake-golangci-executable nil nil nil "cache" "clean"))

(provide 'flymake-collection-golangci)
;;; flymake-collection-golangci.el ends here
