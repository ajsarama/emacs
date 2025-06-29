;;; -*- lexical-binding: t; -*-
(defun ajs/save-captured-buffers ()
  (let ((filename (cdr
		   (assq
		  'filename
		  (bookmark-get-bookmark "org-capture-last-stored")))))
    (with-current-buffer (get-file-buffer filename)
      (save-buffer))))

;; Save the corresponding buffers
(defun ajs/save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda () 
			 (when (member (buffer-file-name) org-agenda-files) 
			   t)))
  (message "Saving org-agenda-files buffers... done"))

;; Add it after refile
;; Isn't there a bookmark for the last refile/capture?
(advice-add 'org-refile :after
	    (lambda (&rest _)
	      (ajs/save-org-buffers)))

(advice-add 'org-capture :after
	    (lambda (&rest _)
	      (ajs/save-org-buffers)))


(setq org-directory "~/org")

;; Inbox is not included here, because entries should always be given status
;; after being refiled
(setq org-agenda-files
      '("tasks.org"))

(setq org-default-notes-file
      (concat org-directory "/inbox.org"))

(ajs/emacs-keybind global-map
  "C-x c" #'org-capture)

(setq org-capture-templates
      `(("t" "Todo" entry (file ,org-default-notes-file)
	 "* TODO %^{Task description}\n%?" :jump-to-captured nil)))

(setq org-refile-targets
      `((,org-agenda-files . (:maxlevel . 1))))

;; These came from the org-modern configuration
(setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-agenda-tags-column 0)

(setq org-todo-keywords
      '((sequence "TODO(t)" "BLOCKED(b)" "|" "DONE(d!)" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-todo) ("BLOCKED" . org-todo)
	("DONE" . org-done) ("CANCELED" . org-done)))

(setq org-startup-indented t
      org-agenda-todo-ignore-with-date t
      org-log-into-drawer t)

(provide 'ajs-org)
