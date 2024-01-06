;;; simple-init -- svitax's simple Emacs configuration
;; Author:
;; Version:
;; URL:
;; Package-Requires: ((emacs "29.1"))
;; This file is not part of GNU Emacs.
;;; Commentary:
;; My config
;;; Code:

;; Basic configuration of built-in features
(setq-default
 enable-recursive-minibuffers t
 inhibit-startup-message t
 initial-scratch-message nil
 native-comp-async-report-warnings-errors nil
 ring-bell-function 'ignore
 scroll-conservatively 1
 use-short-answers 1
 display-line-numbers-type 'relative
 vc-follow-symlinks t
 ;; reduce the clutter in the fringes; we'd like to reserve that space for more
 ;; useful information, like git-gutter and flymake
 indicate-buffer-boundaries t
 indicate-empty-lines nil
 indicate-indicate-buffer-boundaries 'left)
;; display-line-numbers-width 3

;; (load-theme 'modus-vivendi)

(server-start)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(minibuffer-depth-indicate-mode)

(set-face-attribute 'default nil
		    :stipple nil
		    :family "JetBrains Mono Nerd Font"
		    :box nil
		    :strike-through nil
		    :overline nil
		    :underline nil
		    :slant 'normal
		    :weight 'normal
		    :height 160
		    :width 'normal
		    :foundry "nil")

(defun rune/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (let* ((buffer-read-only)
           (image-path "~/.emacs.d/xemacs_color.svg")
           (image (create-image image-path))
           (size (image-size image))
           (height (cdr size))
           (width (car size))
           (top-margin (floor (/ (- (window-height) height) 2)))
           (left-margin (floor (/ (- (window-width) width) 2)))
           (prompt-title "Welcome to Emacs!"))
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n ))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      (insert (make-string (floor (/ (- (window-width) (string-width prompt-title)) 2)) ?\ ))
      (insert prompt-title))
    (setq cursor-type nil)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "q") 'kill-this-buffer)))

(when (< (length command-line-args) 2)
  (add-hook 'emacs-startup-hook (lambda ()
                                  (when (display-graphic-p)
                                    (rune/show-welcome-buffer)))))

(use-package whitespace
  :config
  (global-whitespace-mode)
  :hook
  (prog-mode . (lambda () (setq show-trailing-whitespace t)))
  (text-mode . (lambda () (setq show-trailing-whitespace t)))
  (conf-mode . (lambda () (setq show-trailing-whitespace t)))
  :custom
  (whitespace-style '(face trailing empty big-indent))
  (whitespace-global-modes '(not erc-mode magit-mode)))

(use-package elec-pair
  :hook
  ((prog-mode . electric-pair-local-mode)
   (eval-expression-minibuffer-setup . electric-pair-local-mode)
   (smartparens-mode . (lambda () (electric-pair-local-mode -1)))))

(use-package hl-line
  :hook ((prog-mode . hl-line-mode)
	 (dired-mode . hl-line-mode)
	 (org-mode . hl-line-mode)))

;; (use-package display-line-numbers
;;   :hook ((prog-mode . display-line-numbers-mode)
;; 	 (dired-mode . display-line-numbers-mode)
;; 	 (org-mode . display-line-numbers-mode)))

(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  :config
  ;; TODO: document escape for 'keyboard-escape-quit
  (global-set-key [escape] 'keyboard-escape-quit)
  ;; Since I let `evil-mode' take over `C-u' for buffer scrolling, I need to rebind the
  ;; `universal-argument' command to another key sequence.
  (global-set-key (kbd "C-M-u") 'universal-argument)

  (evil-define-key 'normal 'global (kbd "C-s") 'save-buffer)

  (evil-define-key 'normal 'global (kbd "g l") 'evil-end-of-line)
  (evil-define-key 'normal 'global (kbd "g h") 'evil-first-non-blank)

  (evil-define-key 'normal 'global (kbd "M-h") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "M-j") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "M-k") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "M-l") 'evil-window-right)

  (evil-set-undo-system 'undo-redo)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode)
  (define-key evil-commentary-mode-map (kbd "C-/") 'evil-commentary-line))

(use-package evil-surround
  :config
  (evil-define-key 'operator evil-surround-mode-map "r" 'evil-surround-edit)
  (evil-define-key 'visual evil-surround-mode-map "R" 'evil-surround-edit)
  (evil-define-key 'operator evil-surround-mode-map "s" nil)
  (global-evil-surround-mode 1))

;; save minibuffer history and other things but I only care about the minibuffer history
(use-package savehist
  :hook (after-init . savehist-mode)
  :custom
  (history-delete-duplicates t))

;; save place in file you are currently in
(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-w" . vertico-directory-delete-word)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package consult
  :custom
  (consult-narrow-key "<")
  :bind
  (:map minibuffer-local-map
	("C-r" . consult-history)
	;; Scroll through the items with C-n and C-p instead
	("<up>" . previous-history-element)
	("<down>" . next-history-element))
  :init
  ;; use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
	 :map vertico-map
	 ("C-x C-d" . consult-dir)
	 ("C-x C-j" . consult-dir-jump-file))
  ;; :custom
  ;; (consult-dir-default-command #'consult-project-extra-find)
  :init
  (defun consult-dir--zoxide-dirs ()
    "Return list of zoxide dirs."
    (split-string (shell-command-to-string "zoxide query -l") "\n" t))

  ;; a consult source that calls the zoxide-dirs function
  (defvar consult-dir--source-zoxide
    `(:name "zoxide"
            :narrow ?z
            :category file
            :face consult-file
            :history file-name-history
            :enabled ,(lambda () (executable-find "zoxide"))
            :items ,#'consult-dir--zoxide-dirs)
    "zoxide directory source for `consult-dir'.")
  :config
  ;; adding to the list of consult-dir sources
  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t))

(use-package project
  :ensure nil
  ;; https://andreyor.st/posts/2022-07-16-project-el-enchancements
  ;; A great article about nice enhancements to `project.el'
  ;; One is a custom function that searches for a project root a bit differently
  ;; from the default. It searches for specific root-marker files which in
  ;; practice helps to not have to call `project-try-vc', giving us very
  ;; predictable project roots.
  ;; Another is a custom function to only save modified file-visiting buffers
  ;; in the current project and advices for `project-compile' and `recompile'
  ;; to prevent them from asking to save unrelated buffers.
  )

;; TODO: add easier keybinds for most used tabspaces commands
;; `tabspaces-switch-or-create-workspace' and `tabspaces-open-or-create-project-and-workspace'
(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  :config
  ;; Have only workspace buffers appear in `consult-buffer' by default
  ;; Hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; Set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name "Workspace Buffers"
	  :narrow ?w
	  :history 'buffer-name-history
	  :category 'buffer
	  :state #'consult--buffer-state
	  :default t
	  :items (lambda () (consult--buffer-query
			     :predicate #'tabspaces--local-buffer-p
			     :sort 'visibility
			     :as #'buffer-name)))

    "Set workspace buffer list for `consult-buffer'.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace)

  ;; Sometimes we wish to switch to some open buffer in a tabspace and switch
  ;; to that tab as well. `tabspaces-switch-buffer-and-tab' achieves this.
  ;; If the buffer is open in more than one workspace the user will be prompted
  ;; to choose which tab to switch to. If there is no such buffer users will be
  ;; prompted on whether to create it in a new workspace or the current one.

  ;; BUG: `tabspaces-switch-buffer-and-tab' is unreliable if you're using
  ;; previews in Consult, which implicitly opens the buffer in the tabspace.
  ;; That causes it to not recognize buffers as belonging to their appropriate
  ;; workspace, and prompts me to create a new one with "Select tab" while
  ;; providing no completions.

  ;; Embark provides an elegant way to use `tabspaces-switch-buffer-and-tab'
  ;; while using `consult-buffer'. Narrow to the buffer you want to act on.
  ;; Run `embark-act' and then choose "B" to switch to the buffer and tab.
  ;; (defvar-keymap embark-tabspaces-actions
  ;;   :doc "Keymap for actions with `tabspaces'."
  ;;   :parent embark-general-map
  ;;   "B" #'tab-bar-select-tab-by-name)
  ;; (add-to-list 'embark-keymap-alist '(buffer . embark-tabspaces-actions))

  ;; Add a Consult source for all buffers where the action is
  ;;  `tabspaces-switch-buffer-and-tab'.
  (defvar consult--source-tab-buffer
    `(:name "Tab and Buffer"
	    :narrow ?B
	    :history buffer-name-history
	    :category buffer
	    :hidden t
	    :face consult-buffer
	    :items ,(lambda () (consult--buffer-query :sort 'visibility
						      :as #'buffer-name))
	    :action ,#'tabspaces-switch-buffer-and-tab)
    "Buffers candidate source for `consult-buffer' using `tabspaces-switch-buffer-and-tab'.")
  (add-to-list 'consult-buffer-sources consult--source-tab-buffer))

(use-package consult-project-extra)

(use-package projection
  :hook (after-init . global-projection-hook-mode)
  :bind-keymap
  ("C-x P" . projection-map))

(use-package projection-multi)

;; (use-package projection-multi-embark
;;   :after embark
;;   :after projection-multi
;;   :demand t
;;   :config (projection-multi-embark-setup-command-map))

;; (use-package projection-multi-embark)

(use-package corfu
  :custom
  (corfu-auto t) ;; enable auto completion
  (corfu-auto-prefix 1) ; complete with less prefix keys
  (corfu-auto-delay 0.3)
  (corfu-popupinfo-delay '0.2)
  (eldoc-add-command #'corfu-insert)
  :init
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-echo-delay nil
                  corfu-popup-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  :bind
  (:map corfu-map
        ("C-d" . corfu-popupinfo-scroll-down)
        ("C-u" . corfu-popupinfo-scroll-up)
        ("C-t" . corfu-popupinfo-toggle)
        ("M-SPC" . corfu-insert-separator)
        ("TAB" . corfu-insert)
        ("RET" . nil)))

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package kind-icon
  :after corfu
  ;; :custom
  ;; Computes a blend between a nominal background color and the foreground face
  ;; (kind-icon-blend-background t)
  ;; (kind-icon-default-face 'corfu-default)
  :config
  ;; The icons appear too big in the Corfu completion childframe, you can
  ;; adjust height like this
  (plist-put kind-icon-default-style :height 0.6)

  ;; We can use VSCode icons provided by https://github.com/microsoft/vscode-icons
  ;; instead of the default Material icons set
  (setq kind-icon-mapping
	'((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "vscode")
          (boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "vscode")
          (color          "#"   :icon "symbol-color"       :face success                          :collection "vscode")
          (command        "cm"  :icon "chevron-right"      :face default                          :collection "vscode")
          (constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "vscode")
          (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "vscode")
          (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
          (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
          (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
          (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "vscode")
          (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
          (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "vscode")
          (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "vscode")
          (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode")
          (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode")
          (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
          (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "vscode")
          (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
          (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "vscode")
          (operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "vscode")
          (param          "pa"  :icon "gear"               :face default                          :collection "vscode")
          (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "vscode")
          (reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "vscode")
          (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "vscode")
          (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "vscode")
          (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "vscode")
          (text           "tx"  :icon "symbol-key"         :face font-lock-doc-face               :collection "vscode")
          (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
          (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
          (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "vscode")
          (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
          (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
          (t              "."   :icon "question"           :face font-lock-warning-face           :collection "vscode")))

  ;; Enable for completion UIs with margin-formatters capability, i.e. Corfu
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config (setq all-the-icons-dired-monochrome nil))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "a" 'dired-create-empty-file))

(use-package dired-single
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-open
  :config
  (add-to-list 'dired-open-functions #'dired-open-xdg t))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-preview
  :hook (dired-mode . dired-preview-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "gp" 'dired-preview-mode))

(use-package dired-narrow
  :custom
  (dired-narrow-exit-when-1-left t))

(use-package dired-git-info
  :hook (dired-after-readin . dired-git-info-auto-enable)
  :custom
  (dgi-auto-hide-details-p nil)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    ")" 'dired-git-info-mode))

(use-package dired-filter)

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "gc" 'dired-collapse-mode))

(use-package avy
  :custom
  ;; TODO: document avy-timeout-seconds
  (avy-timeout-seconds 0.25)
  :config
  (evil-define-key 'normal 'global (kbd "s") 'evil-avy-goto-char-timer)
  (evil-define-key 'operator 'global (kbd "s") 'evil-avy-goto-char-timer))

(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  (text-mode . flymake-mode)
  :bind (:map flymake-mode-map
	      ("M-n" . flymake-goto-next-error)
	      ("M-p" . flymake-goto-prev-error))
  :config
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
				  (_ (flymake-diagnostic-text d))))) diags "\n"))))))

(use-package flymake-collection
  :after flymake
  :hook (after-init . flymake-collection-hook-setup))

(use-package package-lint-flymake
  :init
  (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup))

;; (use-package flymake-flycheck
;;   :after flymake
;;   :hook (flymake-mode . flymake-flycheck-auto))

;; TODO: these settings don't get applied to Eglot buffers
(use-package eldoc
  :preface
  ;; `eldoc-documentation-compose' and `eldoc-documentation-compose-eagerly'
  ;; help display information from multiple Eldoc sources at the same time.
  ;; The eager option displays results as they come in; the other collates all
  ;; the answers and displays them when they're all ready.
  ;; I like the eager option.
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)

  ;; Eldoc resizes the echo area display which is intrusive. Let's not do that.
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; `eldoc-doc-buffer' opens a dedicated buffer for your documentation.
  ;; This displays what ever is at point. The only annoying thing about it
  ;; is that it prefers to pop open in an existing window.
  ;; Let's tweak its display to force the buffer to appear at the bottom
  ;; of the frame instead with a fixed window height of 4 lines.
  (add-to-list 'display-buffer-alist
	       ;; The buffer name changes depending on the context,
	       ;; and this display rule reflects that.
	       '("^\\*eldoc for" display-buffer-at-bottom
		 (window-height . 4)))

  ;; Skip showing documentation in the echo area and use an `eldoc-doc-buffer'
  ;; window if it is already displayed.
  (setq eldoc-echo-area-prefer-doc-buffer t)

  ;; If you add a function to `eldoc-documentation-functions' (with add-hook)
  ;; then Eldoc will query the functions in the order they're in and source
  ;; documentation from them. Eglot and Flymake support this out of the box,
  ;; but as it's fairly other packages do not.
  ;; See https://masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  ;; for an example function to integrate Flycheck with Eldoc.
  ;; (add-hook 'eldoc-documentation-functions #'rune/flycheck-eldoc nil t)

  :config
  ;; Eldoc detects movement and uses its idle delay `eldoc-idle-delay' to
  ;; determine when to ask its backend documentation functions for information.
  ;; To improve performance, it won't trigger on every command; instead, it
  ;; maintains a list of common interactive commands. If you use things like
  ;; Paredit or Combobulate then it won't display if you interact with one of
  ;; those commands. Luckily there's the `eldoc-add-command-completion' command
  (eldoc-add-command-completions "paredit-")
  (eldoc-add-command-completions "combobulate-"))

(use-package eldoc-box
  :preface
  ;; Add scrolling support to the `eldoc-box' child frame.
  (defconst rune/lsp-help-buffer-keymap
    (let ((map (make-sparse-keymap)))
      (suppress-keymap map t)
      (define-key map (kbd "C-j") #'rune/lsp-help-scroll-down)
      (define-key map (kbd "C-k") #'rune/lsp-help-scroll-up)
      (define-key map (kbd "C-u") #'rune/lsp-help-C-u)
      (define-key map (kbd "C-d") #'rune/lsp-help-C-d)
      map)
    "Keymap in `rune/lsp-help-at-point'")

  (defvar-local rune/lsp-control-deactivate-fn nil
    "non-nil if current buffer can be controlled by `rune/lsp-help-buffer-keymap'.")

  (defun rune/lsp-help-scroll-up ()
    "Scroll up in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (when rune/lsp-control-deactivate-fn
	(with-selected-frame eldoc-box--frame
	  (scroll-down 3)))))
  (defun rune/lsp-help-scroll-down ()
    "Scroll down in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (when rune/lsp-control-deactivate-fn
	(with-selected-frame eldoc-box--frame
	  (scroll-up 3)))))
  (defun rune/lsp-help-C-u ()
    "C-u (`evil-scroll-up') in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (when rune/lsp-control-deactivate-fn
	(with-selected-frame eldoc-box--frame
	  (scroll-down (max 1 (/ (1- (window-height (selected-window))) 2)))))))
  (defun rune/lsp-help-C-d ()
    "C-u (`evil-scroll-down') in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (when rune/lsp-control-deactivate-fn
	(with-selected-frame eldoc-box--frame
	  (scroll-up (max 1 (/ (1- (window-height (selected-window))) 2)))))))
  (defun rune/lsp-help-at-point ()
    "Display documentation of the symbol at point."
    (interactive)
    (eldoc-box-help-at-point)
    (with-current-buffer eldoc-box--buffer
      (setq-local rune/lsp-control-deactivate-fn
		  (set-transient-map rune/lsp-help-buffer-keymap t))))
  :hook
  ;; `evil-collection' sets bindings using `with-eval-after-load'. That means if
  ;; you define an overriding key in a config section that is defined before
  ;; `evil-collection-init' is called, it will be called first and then
  ;; evil-collection will overwrite it. The solution is to use the hook
  ;; `evil-collection-setup-hook' in order for evil-collection to tell you when
  ;; its done with the key binding setup.
  (evil-collection-setup . (lambda (&rest a)
			     (evil-define-key 'normal 'prog-mode-map
			       (kbd "K") 'rune/lsp-help-at-point))))
;; (kbd "K") 'eldoc-box-help-at-point)))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package puni
  :hook ((prog-mode . puni-mode)
	 (eval-expression-minibuffer-setup . puni-mode))
  :init
  ;; TODO find good keybinds for all puni commands
  ;; `puni-kill-line' (default C-k)
  ;; `puni-backward-kill-line' (default C-S-k)
  ;; `puni-forward-sexp' (default C-M-f)
  ;; `puni-backward-sexp' (default C-M-b)
  ;; `puni-beginning-of-sexp' (default C-M-a)
  ;; `puni-end-of-sexp' (default C-M-e)
  ;; `puni-syntactic-forward-punct' (default M-()
  ;; `puni-syntactic-backward-punct' (default M-))
  ;; `puni-squeeze'copies the list around point (which is the part inside the delimiters),
  ;; and deletes the sexp around point (including the delimiters). it can be used to "rewrap" a sexp
  ;; `puni-slurp-forward' and `puni-slurp-backward'
  ;; `puni-barf-forward' and `puni-barf-backward' move delimiters of the sexp around point across sexps around it
  ;; `puni-raise' uses the sexp at point to replace its parent sexp
  ;; `puni-convolute' exchanges the order of application of two closest other forms
  ;; `puni-splice' unwrap the sexp around point
  ;; `puni-split' split the sexp around point into two
  ;; `puni-transpose' swap the sexps before and after point
  ;; `puni-wrap-round', `puni-wrap-square', `puni-wrap-curly',`puni-wrap-angle' wraps sexps with all kinds of brackets
  (evil-define-key '(normal visual operator) 'puni-mode-map (kbd "<return>") 'puni-expand-region)
  (evil-define-key '(normal visual operator) 'puni-mode-map (kbd "S-<return>") 'puni-contract-region))

(use-package rustic
  :custom
  (rustic-lsp-client 'eglot))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package eglot
  :ensure nil
  :custom
  (eglot-events-buffer-size 0) ;; disable eglot logging (improves performance)
  (eglot-autoshutdown t) ;; shutdown server when last managed buffer is killed
  :preface
  ;; Eglot specifically alters the `eldoc-documentation-strategy', so we
  ;; override it with a hook.
  (defun rune/eglot-eldoc-manage ()
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

  (defun rune/add-eglot-hooks (mode-list)
    "Add `eglot-ensure' to modes in MODE-LIST.

The mode must be loaded, i.e. found with `fboundp'. A mode which
is not loaded will not have a hook added, in which case add it
manually with something like this:

`(add-hook 'some-mode-hook #'eglot-ensure)'"
    (dolist (mode-def mode-list)
      (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
	(cond
	 ((listp mode) (rune/add-eglot-hooks mode))
	 (t
	  (when (and (fboundp mode)
		     (not (eq 'clojure-mode mode)) ; prefer cider
		     (not (eq 'lisp-mode mode)) ; prefer sly/slime
		     (not (eq 'scheme-mode mode)) ; prefer geiser
		     )
	    (let ((hook-name (format "%s-hook" (symbol-name mode))))
	      (message "adding eglot to %s" hook-name)
	      (add-hook (intern hook-name) #'eglot-ensure))))))))

  (defun rune/lsp-bin-exists-p (mode-def)
    "Return non-nil if LSP binary of MODE-DEF is found via `executable-find'."
    (let ((lsp-program (cdr mode-def)))
      ;; `lsp-program' is either a list of strings or a function object
      ;; calling `eglot-alternatives'.
      (if (functionp lsp-program)
	  (condition-case nil
	      (car (funcall lsp-program))
	    ;; When an error occurs it's because Eglot checked for a
	    ;; binary and didn't find one among alternatives.
	    (error nil))
	(message "%s" lsp-program)
	(executable-find (car lsp-program)))))

  (defun eglot-auto-ensure-all ()
    "Add `eglot-ensure' to major modes that offer LSP support.

Major modes are only selected if the major mode's associated LSP
binary is detected on the system."
    (when (require 'eglot nil :noerror)
      (rune/add-eglot-hooks
       eglot-server-programs
       ;; rune/lsp-bin-exists-p errors when it gets to rust but i can't figure out why
       ;; TODO: try exec-path-from-shell to fix rune/lsp-bin-exists-p
       ;; TODO: try inheritenv to fix rune/lsp-bin-exists-p
       ;; (seq-filter
       ;; #'rune/lsp-bin-exists-p
       ;; eglot-server-programs)
       )))
  :hook
  ((eglot-managed-mode . rune/eglot-eldoc-manage))
  :init
  (setq-default eglot-workspace-configuration
                '((:rust-analyzer
                   :completion (:callable (:snippets "fill_arguments"))
                   :checkOnSave (:command "clippy" :allTargets :json-false))))
  (eglot-auto-ensure-all))

(use-package apheleia
  :preface
  ;; Sometimes we want to use `eglot-format-buffer' as the command to format the
  ;; buffer on save. The benefit of going through `eglot' rather than using the
  ;; formatter defined by Apheleia is two-fold. A language server's persistance
  ;; means that formats of the same project are reasonably fast in languages with
  ;; high JIT latency, like Julia. Adding a hook to `eglot-managed-mode' to call
  ;; eglot-format-buffer locks up Emacs while the formatter runs. Aphelia's
  ;; approach of running it in the background is much nicer.
  (require 'cl-lib)
  (cl-defun apheleia-indent-eglot-managed-buffer
      (&key buffer scratch callback &allow-other-keys)
    "Copy BUFFER to SCRATCH, then format scratch, then call CALLBACK."
    (with-current-buffer scratch
      (setq-local eglot--cached-server
		  (with-current-buffer buffer
		    (eglot-current-server)))
      (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
	(eglot-format-buffer))
      (funcall callback)))
  :config
  (apheleia-global-mode +1)

  (add-to-list 'apheleia-formatters
	       '(eglot-managed . apheleia-indent-eglot-manager-buffer))
  (add-to-list 'apheleia-mode-alist '(julia-mode . eglot-managed))
  (add-to-list 'apheleia-mode-alist '(rust-mode . eglot-managed)))

(use-package envrc
  :functions envrc-global-mode
  :init (envrc-global-mode))

(use-package compile
  :hook
  ((compilation-filter . rune--colorize-compilation-buffer))
  :custom
  ;; Automatically scroll build output
  (compilation-scroll-output t)
  ;; Skip anything less than error
  (compilation-skip-threshold 2)
  ;; Automatically jump to the first error unconditionally during compilation
  (compilation-auto-jump-to-first-error t)
  ;; Don't hide any output lines
  (compilation-max-output-line-length nil)
  :bind (:map
	 prog-mode-map
	 ("C-c C-c" . rune/compile))
  :config
  (defun rune/compile (prefix)
    (interactive "p")
    (if (and (eq prefix 1)
             ;; Check if command invoked via binding.
             (eq (key-binding (this-command-keys)) this-command)
             (buffer-live-p compilation-last-buffer))
        ;; Retry using last compile command.
        (progn
          (set-buffer compilation-last-buffer)
          (revert-buffer t t))
      ;; Compile using environment caching.
      (let* ((command (compilation-read-command compile-command))
             (project-root (project-root))
             (cache (rune/compile--history-get command))
             (cached-root (nth 0 cache))
             (cached-directory (nth 1 cache))
             (potential-directory (when (and cached-directory
                                             (file-exists-p (concat project-root cached-directory)))
                                    (concat project-root cached-directory)))
             ;; Overriding default-directory for compile command.
             (default-directory (or potential-directory default-directory)))
        (setq rune/compile--command command)
        (setq rune/compile--project-root project-root)
        (setq rune/compile--directory (if project-root
                                          (file-relative-name default-directory project-root)
					default-directory))
        (compile command))))

  (defun rune/compile-forget-command ()
    (interactive)
    (let* ((history (rune/compile--history-read))
           (command (completing-read "Forget compile command: " (map-keys history))))
      (rune/compile--history-write (map-delete history command))))

  (defun rune/compile--history-path ()
    (concat user-emacs-directory ".comphist.el"))

  (defun rune/compile--history-read ()
    (if (not (file-exists-p (rune/compile--history-path)))
        (make-hash-table :test 'equal)
      (with-temp-buffer
        (insert-file-contents (rune/compile--history-path))
        (read (current-buffer)))))

  (defun rune/compile--history-write (hashtable)
    (with-temp-buffer
      (prin1 hashtable (current-buffer))
      (write-file (rune/compile--history-path) nil)))

  (defun rune/compile--history-add (command project-root directory)
    (let* ((history (rune/compile--history-read)))
      (map-put history (string-trim command) (list project-root directory))
      (rune/compile--history-write history)))

  (defun rune/compile--history-get (command)
    (let* ((history (rune/compile--history-read)))
      (map-elt history (string-trim command))))

  (defun rune/compile-cache-env (buffer string)
    (when (and (string-match "finished" string)
               (boundp 'rune/compile--command)
               (boundp 'rune/compile--directory)
               (boundp 'rune/compile--project-root))
      (rune/compile--history-add rune/compile--command
				 rune/compile--project-root
				 rune/compile--directory)
      (makunbound 'rune/compile--command)
      (makunbound 'rune/compile--directory)
      (makunbound 'rune/compile--project-root)))

  (defun rune--colorize-compilation-buffer ()
    (let ((was-read-only buffer-read-only))
      (unwind-protect
	  (progn
	    (when was-read-only
	      (read-only-mode -1))
	    (ansi-color-apply-on-region (point-min) (point-max)))
	(when was-read-only
	  (read-only-mode +1))))))

(use-package compile-multi
  :init
  (setq compile-multi-config '((rust-ts-mode ("cargo run" . "cargo run")
					     ("cargo run --release" . "cargo run --release")
					     ("cargo test" . "cargo test")))))

(use-package consult-compile-multi
  :after compile-multi
  :demand t
  :config (consult-compile-multi-mode))

(use-package compile-multi-all-the-icons
  :after all-the-icons-completion
  :after compile-multi
  :demand t)

;; (use-package compile-multi-embark
;;   :after embark
;;   :after compile-multi
;;   :demand t
;;   :config (compile-multi-embark-mode +1))

;; (use-package multi-compile
;;   :bind (:map prog-mode-map
;; 	      ("C-c C-v" . multi-compile-run))
;;   :init
;;   (setq multi-compile-alist '((rust-ts-mode . (("cargo run" . "cargo run")
;; 					       ("cargo run --release" . "cargo run --release")
;; 					       ("cargo test" . "cargo test")))
;; 			      ("\\.txt\\'") . (("print-filename" . "echo %file-name")))))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-symbol)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :custom
  (which-key-sort-uppercase-first nil)
  :init (which-key-mode 1))

(use-package modus-themes
  :demand
  :init
  (require 'modus-themes)

  (defun rune/modus-themes-tab-bar-colors ()
    "Override tab faces to have even less variety"
    (modus-themes-with-colors
      (custom-set-faces
       `(tab-bar ((,c
		   :height 0.8
		   :background ,bg-main
		   :box nil)))
       `(tab-bar-tab ((,c
		       :background ,bg-main
		       :underline (:color ,blue-intense :style line)
		       :box (:line-width 2 :style flat-button))))
       `(tab-bar-tab-inactive ((,c
				:background ,bg-main
				:box (:line-width 2 :style flat-button)))))))
  (add-hook 'modus-themes-after-load-theme-hook #'rune/modus-themes-tab-bar-colors)

  ;; Generally user options should never be touched by a theme. However, according
  ;; to the maintainer of modus-themes, certain cases like `hl-todo-keyword-faces'
  ;; and the `flymake-*-bitmap' variants merit an exception.
  ;; This is annoying because I don't like the face used for Flymake bitmaps.
  ;; I would like them to not have a background color. These variables need to be
  ;; set after loading the theme.
  (defun rune/modus-themes-flymake-bitmaps ()
    "Override Flymake bitmaps to blend into the fringe"
    (customize-set-variable
     'flymake-error-bitmap '(flymake-double-exclamation-mark compilation-error))
    (customize-set-variable
     'flymake-warning-bitmap '(exclamation-mark compilation-warning))
    (customize-set-variable
     'flymake-note-bitmap '(exclamation-mark compilation-info)))
  (add-hook 'modus-themes-after-load-theme-hook #'rune/modus-themes-flymake-bitmaps)

  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-to-toggle '(modus-vivendi))
  (setq modus-themes-bold-constructs t
	modus-themes-mixed-fonts t

	;; Control the style of command prompts (e.g. minibuffer, shell, IRC clients).
	;; `modus-themes-prompts' are either nil (the default), or a list of
	;; properties that may include any of those symbols: `italic',;; `WEIGHT'
	modus-themes-prompts '(bold))

  ;; Define some palette overrides using the presets
  (customize-set-variable 'modus-themes-common-palette-overrides
			  `(;; To hide the border around the active and inactive mode lines, we
			    ;; set their color to that of the underlying background
			    (bg-mode-line-active bg-inactive)
			    (fg-mode-line-active fg-main)
			    (bg-mode-line-inactive bg-inactive)
			    (fg-mode-line-active fg-dim)
			    (border-mode-line-active bg-inactive)
			    (border-mode-line-inactive bg-main)
			    ;; Change the background of matching parenthesis to a shade of magenta
			    (bg-paren-match bg-magenta-subtle)
			    ;; Enable underlining matching parenthesis by applying a color to them
			    (underline-paren-match fg-main)
			    ;; Make the fringe invisible
			    (fringe unspecified)))

  (defun rune/modus-themes-init ()
    (load-theme (car modus-themes-to-toggle) t))
  :hook (after-init . rune/modus-themes-init))

;; TODO replace doom-nano-modeline with something else so i don't have to build it myself with nix
;; preferably something in ELPA/MELPA
(use-package doom-themes)
(use-package doom-nano-modeline
  :custom
  (doom-nano-modeline-position 'bottom)
  :config
  (doom-nano-modeline-mode 1))

(use-package keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode-line-mode
    "Show current command and its keybinding in the modeline. Fix for use with
doom-modeline"
    :global t
    (if keycast-mode
	(add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '("" keycast-mode-line))

  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing!")))
  (setf (alist-get 'strokes-do-stroke
                   keycast-substitute-alist)
        '("[mouse]" t))

  (defun store-action-key+cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys)
          keycast--this-command cmd)
    cmd)

  (defun store-action-key-no-cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys)
          keycast--this-command cmd))

  (defun keycast-capture-avy-dispatch (char)
    (if-let ((cmd (assoc char avy-dispatch-alist)))
        (setq keycast--this-command-keys (make-vector 1 char)
              keycast--this-command (cdr cmd))))

  (advice-add 'embark-keymap-prompter :filter-return #'store-action-key+cmd)
  ;; (advice-add 'avy-goto-char-timer :filter-return #'store-action-key+cmd)
  (advice-add 'avy-handler-default :before #'keycast-capture-avy-dispatch)

  (defun force-keycast-update (&rest _)
    (force-mode-line-update t))

  (dolist (cmd '(embark-act embark-become))
    (advice-add cmd :before #'force-keycast-update)))

;; (use-package git-gutter
;;   :preface
;;   (defun modus-themes-custom-faces ()
;;     (modus-themes-with-colors
;;       (custom-set-faces
;;        `(git-gutter-fr:added ((,class :foreground ,green-fringe-bg)))
;;        `(git-gutter-fr:deleted ((,class :foreground ,red-fringe-bg)))
;;        `(git-gutter-fr:modified ((,class :foreground ,yellow-fringe-bg))))))
;;   :hook ((prog-mode . git-gutter-mode)
;; 	 (text-mode . git-gutter-mode)
;; 	 (modus-themes-after-load-theme . modus-themes-custom-faces))
;;   :config
;;   (setq git-gutter:update-interval 0.05))

;; (use-package git-gutter-fringe
;;   :config
;;   (setq git-gutter-fr:side 'right-fringe)
;;   (modus-themes-load-vivendi))

(use-package magit
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; TODO: better theme for delta in git decorations.
;; copy from modus-theme magit diff faces
;; (use-package magit-delta
;;   :hook (magit-mode . magit-delta-mode))

(use-package general
  :after evil
  :init
  (setq evil-mode-line-format '(before . mode-line-front-space))
  :config (general-evil-setup))

;; set up 'SPC' as the leader key
(general-create-definer leader-def
  :states '(normal visual insert emacs)
  :keymaps 'override
  :prefix "SPC" ; set leader
  :global-prefix "M-SPC") ; access leader in insert mode

;; set up ',' as the local leader key
(general-create-definer local-leader-def
  :states '(normal visual insert emacs)
  :keymaps 'override
  :prefix "," ; set leader
  :global-prefix "M-,") ; access leader in insert mode

(leader-def
  "a" '(eglot-code-actions :which-key "Code actions")
  "b" '(consult-buffer :which-key "Buffer switcher")
  ;; "c" '( :which-key "")
  ;; "d" '( :which-key "Debug")
  "e" '(dired-jump :which-key "File explorer") ;; TODO
  "E" '(find-file :which-key "File browser") ;; TODO
  "f" '(consult-project-extra-find :which-key "File picker") ;; TODO
  "g" '(magit-status :which-key "Git status")
  "h" '(help-command :which-key "Help")
  ;; "i" '( :which-key "")
  "j" '(consult-dir :which-key "Jump to directory")
  "k" '(apheleia-format-buffer :which-key "Format buffer") ;; TODO
  "l" '(consult-line :which-key "Search buffer lines")
  "m" '(projection-multi-compile :which-key "Multi-compile")
  ;; "n" '( :which-key "")
  ;; "o" '( :which-key "")
  "p" '(:ignore t :which-key "Project")
  ;; "p" '(tabspaces-command-map :which-key "Projects")
  "q" '(kill-emacs :which-key "Kill Emacs")
  "r" '(eglot-rename :which-key "Rename symbol")
  "s" '(consult-eglot-symbols :which-key "Symbol picker")
  "t" '(project-eshell :which-key "Eshell")
  ;; "u" '( :which-key "")
  ;; "v" '( :which-key "")
  ;; "w" '( :which-key "Windows")
  "x" '(consult-flymake :which-key "Diagnostics")
  "y" '(consult-yank-pop :which-key "Kill-ring")
  ;; "z" '( :which-key "")
  "SPC" '(execute-extended-command :which-key "M-x")
  "," '(project-compile :which-key "Compile")
  "." '(recompile :which-key "Recompile")
  ;; ";" '( :which-key "")
  ;; "'" '( :which-key "")
  ;; "-" '( :which-key "")
  ;; "|" '( :which-key "")
  ;; "" '(consult-global-mark :which-key "Mark-ring") ;; TODO
  "/" '(consult-grep :which-key "Grep"))

(leader-def
  "pb" '(tabspaces-switch-to-buffer :which-key "tabspaces-switch-to-buffer")
  "pd" '(tabspaces-close-workspace :which-key "tabspaces-close-workspace")
  "pk" '(tabspaces-kill-buffers-close-workspace :which-key "tabspaces-kill-buffer-close-workspaces")
  "po" '(tabspaces-open-or-create-project-and-workspace :which-key "tabspaces-open-or-create-project-and-workspace")
  "pr" '(tabspaces-remove-current-buffer :which-key "tabspaces-remove-current-buffer")
  "ps" '(tabspaces-switch-or-create-workspace :which-key "tabspaces-switch-or-create-workspace")
  "pt" '(tabspaces-switch-buffer-and-tab :which-key "tabspaces-switch-buffer-and-tab")
  "pC" '(tabspaces-clear-buffers :which-key "tabspaces-clear-buffers")
  "pR" '(tabspaces-remove-selected-buffer :which-key "tabspaces-remove-selected-buffer"))
