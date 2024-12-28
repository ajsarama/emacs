;;; -*- lexical-binding: t -*-
;;;; Theme

;; Palette overrides
;; Remove ugly brown level 2 headings
(defvar ajs/operandi-overrides
  '((fg-heading-2 indigo)
    (comment magenta-warmer)
    (bg-paren-match fg-main)
    (fg-paren-match bg-main))
  "Overrides for modus-operandi theme.")

(defvar ajs/vivendi-overrides
  '((comment yellow-warmer))
  "Overrides for modus-vivendi theme.")

(defun ajs/modus-face-attributes ()
  "Change face attributes from their defaults."
  (if (boundp 'org-tag)
      (set-face-attribute 'org-tag nil :height 0.75 :weight 'light)
    (with-eval-after-load 'org
      (set-face-attribute 'org-tag nil :height 0.75 :weight 'light))))

;; Theme loading and set keybindings
(use-package modus-themes
  :demand t
  :bind (("<f5>" . modus-themes-toggle))
  :config
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
	modus-themes-mixed-fonts t
	modus-themes-prompts '(bold)
	modus-themes-completions '((matches . (bold))
				   (selection . (italic)))
	modus-themes-headings '((0 . (variable-pitch 1.6))
				(1 . (variable-pitch 1.4))
				(2 . (variable-pitch 1.2))
				(t . (variable-pitch 1.0)))
	modus-operandi-tinted-palette-overrides ajs/operandi-overrides
	modus-vivendi-tinted-palette-overrides ajs/vivendi-overrides)
  (ajs/modus-face-attributes)
  (add-hook 'modus-themes-after-load-theme-hook #'ajs/modus-face-attributes))

(modus-themes-select 'modus-operandi-tinted)

;;;; Spacious Padding

(use-package spacious-padding
  :demand t
  :config
  (setq spacious-padding-subtle-mode-line t)
  (spacious-padding-mode 1))

;;;; Font and Nerd Icons

;; Font Setup
;; Name of the font to use for code.
;; MUST be a Nerd Font.
(defvar ajs/fixed-pitch-font "UbuntuSansMono Nerd Font Mono"
  "Font to use as default font for code.")

;; Name of the variable width font to use for prose-heavy modes like Org.
(defvar ajs/variable-pitch-font "UbuntuSans Nerd Font"
  "Font to use for variable-width buffers, like Org mode.")

;; Default font size to use on Emacs init.
(defvar ajs/default-font-size 14
  "Font point to use as the default")

;; Sets the fonts using the provided pt.
(let ((font-suffix (concat "-" (number-to-string ajs/default-font-size))))
  (set-face-attribute 'default nil :font (concat ajs/fixed-pitch-font font-suffix))
  (set-face-attribute 'fixed-pitch nil :font (concat ajs/fixed-pitch-font font-suffix))
  (set-face-attribute 'variable-pitch nil :font (concat ajs/variable-pitch-font font-suffix)))


;; Nerd Icons
(use-package nerd-icons
  :demand t
  :config
  (setq nerd-icons-font-family ajs/fixed-pitch-font
	nerd-icons-scale-factor 1.0))

;;;; Olivetti Mode

(use-package olivetti
  :config
  (setq-default olivetti-body-width 100
	olivetti-style 'fancy))

(provide 'ajs-visuals)
