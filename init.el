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
  "C-c c" #'compile)

(global-visual-line-mode 1)
(which-key-mode 1)
(blink-cursor-mode -1)
(recentf-mode 1)

;; Elpaca installer
(defvar elpaca-installer-version 0.10)
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
(if (file-exists-p (expand-file-name "lisp/sumo" user-emacs-directory))
    (progn (add-to-list 'load-path (expand-file-name "lisp/sumo" user-emacs-directory))
	   (require 'sumo))
  (use-package sumo
    :ensure (:host github :repo "ajsarama/sumo" :inherit nil)))

(load-theme 'sumo-dark t)

;;; Faces
(use-package fontaine
  :ensure t
  :demand t
  :config
  (setq fontaine-presets
	;; Presets for fonts that can be enabled
	;; Probably looking at a programming, prose, screen sharing, and multi-window
      '((regular
         :default-family "Iosevka"
         :default-height 140
         :fixed-pitch-family "Iosevka"
         :variable-pitch-family "Aporetic Sans"
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

