;;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold (* 1000 1000 8))

(setq bookmark-fringe-mark nil)

(setq make-backup-files nil)
(setq-default backup-inhibited t)
(setq create-lockfiles nil)

(setq custom-file (make-temp-file "emacs-custom-"))

(setq custom-safe-themes t)

(require 'cl-lib)
;;; Keybind Macro
(defmacro ajs/emacs-keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  (let ((keys (seq-filter #'stringp definitions))
        ;; We do accept nil as a definition: it unsets the given key.
        (commands (seq-remove #'stringp definitions)))
    `(when-let* (((keymapp ,keymap))
                 (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (let* ((key (car pair))
                   (command (cdr pair)))
              (unless (and (null key) (null command))
                `(define-key map (kbd ,key) ,command))))
          (cl-mapcar #'cons keys commands)))))

(defun ajs/find-char (char)
  "Move point to the next occurrence of CHAR in the current line.
If CHAR is not found, print a message and leave point unchanged."
  (interactive "cFind character: ")
  (let ((start (point))
        (line-end (line-end-position))
	(case-fold-search nil))
    (forward-char)
    (if (search-forward (char-to-string char) line-end t)
        (backward-char)
      (progn
        (goto-char start)
        (message "Character '%c' not found on this line." char)))))

(defun ajs/find-char-backward (char)
  "Move point to the next occurrence of CHAR in the current line.
If CHAR is not found, print a message and leave point unchanged."
  (interactive "cFind character: ")
  (let ((start (point))
        (line-start (line-beginning-position))
	(case-fold-search nil))
    (unless (search-backward (char-to-string char) line-start t)
      (progn
        (goto-char message "Character '%c' not found on this line." char)))))

(defun ajs/increment-integer-at-point ()
  (interactive)
  (let* ((original (char-after))
	 (character (- original 48)))
    (when (and (>= character 0) (<= character 8))
      (delete-backward-char -1)
      (insert-char (1+ original))
      (backward-char))))

(ajs/emacs-keybind global-map
  "M-o" #'other-window
  "M-l" #'display-line-numbers-mode
  "M-f" #'ajs/find-char
  "M-b" #'ajs/find-char-backward
  "C-z" #'ajs/increment-integer-at-point
  "C-c c" #'compile
  "C-c g" #'magit-dispatch
  "C-c f" #'magit-file-dispatch)

(global-visual-line-mode 1)
(which-key-mode 1)
(blink-cursor-mode -1)
(recentf-mode 1)

;; Elpaca installer
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))


;;; Custom Lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package modus-themes
  :ensure t
  :config
  (modus-themes-select 'modus-operandi))

;;; Faces
(use-package fontaine
  :ensure t
  :demand t
  :config
  (setq fontaine-presets
	;; Presets for fonts that can be enabled
	;; Probably looking at a programming, prose, screen sharing, and multi-window
      '((regular
         :default-family "Iosevka Custom"
         :default-height 140
         :fixed-pitch-family "Iosevka Custom"
         :variable-pitch-family "Iosevka Custom"
         :line-spacing 0)
        (large
         :default-height 230
         :line-spacing 1)))
  ; Tries to restore the last font preset that was used
  (fontaine-mode 1)
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

;;; Spacious padding
(use-package spacious-padding
  :ensure t
  :config
  (spacious-padding-mode 1))

;;; Olivetti
(use-package olivetti
  :ensure t
  :config
  (setq-default olivetti-body-width 0.7
		olivetti-style nil)
  (custom-set-faces '(olivetti-fringe ((nil :inherit nil :foreground nil :background nil)))))

;;; Simple C Mode
(use-package simpc-mode
  :ensure (:host github :repo "rexim/simpc-mode" :inherit nil)
  :config
  ;; Takes precedence over CC mode (yuck!)
  (add-to-list 'auto-mode-alist '("\\.c\\'" . simpc-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . simpc-mode)))

;;; Elmine fork
(use-package elmine
  :ensure (:host github :repo "ajsarama/elmine" :inherit nil))

;;; Auto-matching
(use-package smartparens
  :ensure smartparens  ; install the package
  :hook (prog-mode markdown-mode comint-mode) ; add `smartparens-mode` to these hooks
  :config
  ; load default config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil))

;;; Minibuffer packages
(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;; Example configuration for Consult
(use-package consult
  :ensure t
  :demand t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package savehist
  :ensure nil
  :config
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

;;; Markdown mode
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;;; Eat terminal emulator and Eshell
(use-package eat
  :ensure t
  :config
  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

;;; Dired configuration
(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t)
  (ajs/emacs-keybind dired-mode-map
    "M-+" #'dired-create-empty-file))

(require 'ajs-org)
(require 'ajs-meow)

;;; Formatting
(use-package reformatter
  :ensure t
  :config
  (reformatter-define clang-format
		      :program "clang-format"
		      :args '("-style={IndentWidth: 4, TabWidth: 4}")
		      :lighter " clang-format"))


;;; Atuin for Eshell
(use-package eshell-atuin
  :ensure t
  :after eshell
  :config
  (eshell-atuin-mode)
  (ajs/emacs-keybind eshell-mode-map
    "M-<up>" #'eshell-atuin-history))

;;; Common Lisp
(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))

;;; Jumping
(use-package avy
  :ensure t
  :config
  (ajs/emacs-keybind global-map
    "M-j" #'avy-goto-char-timer))

;;; Colors
(use-package rainbow-mode
  :ensure t)

;;; Transient
(use-package transient
  :ensure t)

;;; Magit
(use-package magit
  :after (transient)
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode clojurescript-mode clojurec-mode) . lsp)
  :commands lsp
  :config
  (setq lsp-enable-indentation nil   ;; you might prefer clojure-mode's indentation
        lsp-enable-on-type-formatting nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package haskell-mode
  :ensure t)

(use-package info
  :ensure nil
  :config
  (add-to-list 'Info-directory-list "/opt/homebrew/Cellar/emacs-plus@31/31.0.50/share/info/emacs"))

(use-package clojure-mode
  :ensure t
  :demand t)

(use-package consult-lsp
  :ensure t
  :demand t)

(use-package flycheck
  :ensure t)

(use-package consult-flycheck
  :ensure t)
