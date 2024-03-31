;;; flymake-extras.el -*- lexical-binding: t; -*-

(defun flymake-eldoc-function (report-doc &rest _)
  "Document diagnostics at point.
Intended for `eldoc-documentation-functions' (which see)."
  (let ((diags (flymake-diagnostics (point))))
    (when diags
      (funcall report-doc
               (mapconcat (lambda (d)
                            (let ((level (flymake-diagnostic-type d)))
                              (pcase level
                                ;; TODO: warning, error, and note don't work in non eglot modes (elisp)
                                ('warning (propertize (flymake-diagnostic-text d) 'face 'flymake-warning-echo))
                                ('error (propertize (flymake-diagnostic-text d) 'face 'flymake-error-echo))
                                ('note (propertize (flymake-diagnostic-text d) 'face 'flymake-note-echo))
                                ('eglot-warning (propertize (flymake-diagnostic-text d) 'face 'flymake-warning-echo))
                                ('eglot-error (propertize (flymake-diagnostic-text d) 'face 'flymake-error-echo))
                                ('eglot-note (propertize (flymake-diagnostic-text d) 'face 'flymake-note-echo))
                                (_ (flymake-diagnostic-text d))))) diags "\n")))))

(provide 'flymake-extras)
