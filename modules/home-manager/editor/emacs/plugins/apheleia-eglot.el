;;; apheleia-eglot.el --- Format buffers on save using eglot -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Svitax Erevos

;; Author: Svitax Erevos <svitax@not.real>
;; Keywords: tools, lsp
;; Package-Requires: ((emacs "29.0") (eglot "1.16") (apheleia "4.1"))
;; Version: 0.1

;; Copyright (C) 2024  Svitax Erevos

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package provides a formatter for `apheleia' that asynchronously queries
;; a language server, managed by `eglot', to edit the current buffer.

;; The benefit of going through Eglot rather than using the formatter provided by Apheleia is two-fold:

;; 1. A language server's persistance means that formats of the same project
;; are reasonably fast for languages with high JIT latency, like Julia.
;; 2. Adding a hook to `eglot-managed-mode' to call `eglot-format-buffer' locks
;; up Emacs while the formatter runs. Apheleia's approach of running it in the
;; background provides a much nicer user experience.

;;; Code:
(require 'cl-lib)
(require 'apheleia)
(require 'eglot)

;;;###autoload
(cl-defun apheleia-eglot (&key buffer scratch callback &allow-other-keys)
  "Apheleia formatter using a running language server managed by Eglot.
This copies BUFFER to SCRATCH, calls `eglot-format-buffer' on SCRATCH using
the server associated with BUFFER before calling CALLBACK."
  (with-current-buffer scratch
    (setq-local eglot--cached-server
		(with-current-buffer buffer
		  (eglot-current-server)))
    (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
      ;; (eglot-format-buffer)
      ;; There are no custom vars to make `eglot-format-buffer' silent, but the
      ;; third argument to `eglot-format' (which eglot-format-buffer calls) gets
      ;; passed to `eglot--apply-text-edits' as the SILENT argument.
      (eglot-format nil nil t))
    (funcall callback)))

;; Automatically add apheleia-eglot to apheleias formatter repository.
;;;###autoload
(with-eval-after-load 'apheleia (push '(eglot . apheleia-eglot) apheleia-formatters))

;;;###autoload
(define-minor-mode apheleia-eglot-mode
  "Minor mode for formatting buffers with Apheleia and Eglot.
All this mode does is ensure Eglot is the first choice formatter
for the current `major-mode', to enable format on save you should
also enable `apheleia-mode'."
  :lighter nil
  (if apheleia-eglot-mode
      (progn
        (make-variable-buffer-local 'apheleia-mode-alist)
        (push (cons major-mode 'eglot)
              apheleia-mode-alist))
    (cl-delete (cons major-mode 'eglot) apheleia-mode-alist)))

(provide 'apheleia-eglot)
;;; apheleia-eglot.el ends here
