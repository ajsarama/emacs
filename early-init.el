;;; -*- lexical-binding: t; -*-

;; Do not use package.el in favor of elpaca
(setq package-enable-at-startup nil)

;; Set properties, taken from Prot.
;; https://protesilaos.com/emacs/dotemacs#h:7b7b5898-09f7-4128-8af0-4041f67cb729
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      ; dialog boxes for mouse events
      use-dialog-box nil
      use-file-dialog nil
      ; y or n instead of yes or no
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)

;; Disable default utilities
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Disable native comp warnings
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors nil))
