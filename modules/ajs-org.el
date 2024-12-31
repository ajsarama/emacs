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
	org-enforce-todo-dependencies t
	org-log-into-drawer "LOGBOOK"
	org-agenda-files '("~/org/work.org")
	org-refile-use-outline-path nil
	org-log-done 'time
	org-archive-subtree-save-file-p t
	org-archive-save-context-info '(file))
  (setq org-refile-targets
      '((org-agenda-files :maxlevel . 10)))
  (setq org-todo-keywords
	'((sequence "REOPEN(o@)" "TODO(t!)" "RESEARCH(r)" "WAIT(w@)" "CODEREVIEW(c!)"
		    "|" "DONE(d)" "CANCELED(x@)")
	  (sequence "BUG(b!)" "NOREPRODUCE(p)" "|" "FIXED(f)" "CLOSED(l)")))
  (setq org-capture-templates
	'(("d" "Development" entry (file "~/org/inbox.org")
	   "* Development %U\n%?" :empty-lines 1)
	  ("m" "Meeting" entry (file "~/org/inbox.org")
	   "* %^{Meeting:|Hydro|Hydro|Weekly|All-Hands} Meeting %^T\n%?" :empty-lines 1)
	  ))
  )

(ajs/emacs-keybind global-map
  "C-c c" #'org-capture
  "C-c i" (lambda () (interactive) (find-file "~/org/inbox.org")))

(provide 'ajs-org)
