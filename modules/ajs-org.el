;;;; Initial setup for the cursor and variable pitch fonts
(defun ajs/org-mode-visual-setup ()
  "Change the cursor type and enable variable pitch mode."
  ;; Set the cursor to a bar (vertical line)
  (setq cursor-type 'bar)
  
  ;; Enable variable-pitch-mode for proportional fonts
  (variable-pitch-mode 1)
  (olivetti-mode 1))

;;;; Basic option setup
(use-package org
  :config
  (add-hook 'org-mode-hook 'ajs/org-mode-visual-setup)
  (setq org-special-ctrl-a t
	org-tags-column 0
	org-directory "~/org"
	org-default-notes-file "~/org/inbox.org"
	))


(provide 'ajs-org)
