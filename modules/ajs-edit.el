;;; -*- lexical-binding: t -*-
;;;; Visual Undo
;; Use unicode symbols for the tree instead of ASCII
;; Set the undo limit to be something decently large so we can actually use it
(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (setq undo-limit 67108864) ; 64mb.
  (setq undo-strong-limit 100663296) ; 96mb.
  (setq undo-outer-limit 1006632960) ; 960mb
  (ajs/emacs-keybind global-map
    "C-/" #'vundo))

;;;; Shift-Page Motions
(ajs/emacs-keybind global-map
  "S-<next>" (lambda () (interactive)
	       (scroll-up
			(/ (window-height) 2))))

(ajs/emacs-keybind global-map
  "S-<prior>" (lambda () (interactive)
		(scroll-down
			(/ (window-height) 2))))

;;;; Hydra
(use-package hydra)


;;;; Outline Mode
(defhydra hydra-outline ()
  "outline:"
  ("SPC" outline-cycle "cycle")
  ("b" outline-cycle-buffer "buffer")
  ("a" outline-show-all "all")
  ("s" outline-show-subtree "subtree")
  ("h" outline-show-only-headings "headings")
  ("<up>" outline-previous-visible-heading "up")
  ("<down>" outline-next-visible-heading "down")
  ("o" outline-hide-other "hide"))

(use-package outline
  :demand t
  :config
  ;; Automatically enable `outline-minor-mode` in programming modes
  (add-hook 'prog-mode-hook #'outline-minor-mode)
  (add-hook 'org-mode-hook #'outline-minor-mode)
  ;; Define keybinding for `outline-minor-mode` only when the mode is loaded
  (add-hook 'outline-minor-mode-hook
            (lambda ()
              (define-key outline-minor-mode-map (kbd "C-c o") 'hydra-outline/body)))
  
  ;; Enable `outline-minor-mode` for already-open buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or (derived-mode-p 'prog-mode) (derived-mode-p 'org-mode))
        ;; Enable outline-minor-mode if not already enabled
        (unless (bound-and-true-p outline-minor-mode)
          (outline-minor-mode 1))))))


;;;; Paredit




(provide 'ajs-edit)
