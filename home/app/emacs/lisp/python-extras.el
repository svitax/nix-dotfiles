;;; python-extras.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my-py-hl-filter-self (node)
  (not (equal (treesit-node-text node) "self")))

;;;###autoload
(defun my/python-update-highlights ()
  (setq python--tresit-settings
	(append python--treesit-settings
		(treesit-font-lock-rules
		 ;; custom rules
		 :language 'python
		 :override 'nil
		 :feature 'custom
		 '((call function: (identifier) @my-font-lock-function-call-face))

		 :language 'python
		 :override 't
		 :feature 'custom
		 '((call function: (attribute attribute: (identifier) @my-font-lock-method-call-face)))

		 :language 'python
		 :override 't
		 :feature 'custom
		 '(((identifier) @my-font-lock-global-var-face
		    (:match "^_?[A-Z][A-Z_0-9]*$" @my-font-lock-global-var-face)))

		 :language 'python
                 :override 't
                 :feature 'custom
                 '((call function: (identifier) @my-font-lock-constructor-face
                    (:match "^_?[A-Z]" @my-font-lock-constructor-face))
                   (call function: (attribute attribute: (identifier) @my-font-lock-constructor-face)
                         (:match "^_?[A-Z]" @my-font-lock-constructor-face)))

		 :language 'python
                 :feature 'custom
                 '((keyword_argument name: (identifier) @my-font-lock-argument-keyword-face))

                 :language 'python
                 :feature 'custom
                 '(((parameters (identifier) @my-font-lock-parameter-face
                     (:pred my-py-hl-filter-self @my-font-lock-parameter-face)))

                   (parameters (typed_parameter (identifier) @my-font-lock-parameter-face))
                   (parameters (default_parameter name: (identifier) @my-font-lock-parameter-face))
                   (parameters (typed_default_parameter name: (identifier) @my-font-lock-parameter-face))

                   (parameters
                    (list_splat_pattern ; *args
                     (identifier) @my-font-lock-parameter-face))
                   (parameters
                    (dictionary_splat_pattern ; **kwargs
                     (identifier) @my-font-lock-parameter-face))

                   (lambda_parameters
                    (identifier) @my-font-lock-parameter-face))

                 :language 'python
                 :feature 'custom
                 '((argument_list (identifier) @my-font-lock-argument-face)
                   (argument_list
                    (list_splat         ; *args
                     (identifier) @my-font-lock-argument-face))
                   (argument_list
                    (dictionary_splat   ; **kwargs
                     (identifier) @my-font-lock-argument-face)))
                 :language 'python
                 :override 't
                 :feature 'custom
                 '((list_splat_pattern "*" @font-lock-misc-punctuation-face)
                   (list_splat "*" @font-lock-misc-punctuation-face)
                   (dictionary_splat_pattern "**" @font-lock-misc-punctuation-face)
                   (dictionary_splat "**" @font-lock-misc-punctuation-face))))))

;;;###autoload
(defun my/python-setup-highlight ()
  (unless (member 'custom (nth 2 treesit-font-lock-feature-list))
    (push 'custom (nth 2 treesit-font-lock-feature-list))
    (push 'font-lock-misc-punctuation-face (nth 2 treesit-font-lock-feature-list)))

  (setopt treesit-font-lock-level 4))

;;;###autoload
(defun my/python-setup ()
  (my/python-setup-highlight))

(defgroup my/code-faces nil
  "Faces for highlighting code.")

;;;###autoload
(defface my-font-lock-constructor-face
  '((default :inherit font-lock-type-face :slant oblique))
  "Face for creating new instances."
  :group 'my/code-faces)

;;;###autoload
(defface my-font-lock-global-var-face
  '((default :inherit font-lock-constant-face :weight bold))
  "Face for constants."
  :group 'my/code-faces)

;;;###autoload
(defface my-font-lock-function-call-face
  '((default :foreground "#000080" :weight semi-bold))
  "Face for function call"
  :group 'my/code-faces)

;;;###autoload
(defface my-font-lock-method-call-face
  '((default :inherit my-font-lock-function-call-face :slant oblique))
  "Face for method call"
  :group 'my/code-faces)

;;;###autoload
(defface my-font-lock-command-face
  '((default :foreground "#000080"))
  "Face for method call"
  :group 'my/code-faces)

;;;###autoload
(defface my-font-lock-special-var-face
  '((default :inherit font-lock-variable-name-face :weight bold))
  "Face for special var"
  :group 'my/code-faces)

;;;###autoload
(defface my-font-lock-argument-face
  ;; '((default :foreground "#8b7765"))
  '((default :foreground "#77002a"))
  "Face for argument"
  :group 'my/code-faces)

;;;###autoload
(defface my-font-lock-argument-keyword-face
  '((default :foreground "#8b7765"))
  "Face for keyword"
  :group 'my/code-faces)

;;;###autoload
(defface my-font-lock-parameter-face
  ;; #8b7765
  '((default :foreground "#8b7765"))
  "Face for parameter"
  :group 'my/code-faces)

;;;###autoload
(defun my/setup-code-faces ()
  (custom-set-faces
   '(font-lock-property-use-face ((t (:slant oblique))))
   '(font-lock-misc-punctuation-face ((t (:foreground "#b03060" :weight semi-bold))))))

(provide 'python-extras)
