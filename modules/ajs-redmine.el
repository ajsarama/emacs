;;; -*- lexical-binding: t; -*-
;;;; Redmine Configuration
;; The following functionality links org-mode with Redmine.
;; There are many such packages, but this one is mine.
;; It is based on the `elmine' library:
;; `https://github.com/leoc/elmine'
;;
;; This is only for retrieving the contents of Redmine issues and pudding them in an org buffer.
;; I do not use this module for updating my Redmine tasks.
(require 'elmine)

;;;; Variables and constants
(with-eval-after-load 'org
  (defvar ajs/redmine-inbox
    (concat org-directory "/inbox.org")
    "Place to dump Redmine issues to be refiled.")

  (defvar ajs/redmine-org-files
    `(,(concat org-directory "/work.org")
      ,(concat org-directory "/work-archive.org"))
    "List of files to check for existing redmine issue entries"))

(defconst ajs/redmine-property-symbol
  (intern ":REDMINE")
  "Symbol for the redmine entry in the property drawer")

(defvar ajs/secrets-file
  (concat user-emacs-directory "lisp/secrets.el.gpg")
  "Secret file to hold sensitive data.")

;;;; Parsing functions
;; Get AST from an org file. Used to check for existing redmine issues.
(defun ajs/parse-org-file (path)
  "Parse the provided filepath using `org-element-parse-buffer' in a temporary buffer."
  (if (file-exists-p path)
      (with-temp-buffer
	(org-mode)
	(insert-file-contents path)
	(org-element-parse-buffer))
    nil))

;; Attempt to get the `redmine' property from an org headline object.
(defun ajs/redmine-from-headline (headline)
  "Get a redmine id from a headline, `nil' if it doesn't have one."
  (let ((redmine-prop (org-element-property
		       ajs/redmine-property-symbol headline)))
    (if redmine-prop redmine-prop nil)))

;; Get the redmine issues that already exist in my org files.
;; TODO: make this more efficient once my org file structure has matured.
(defun ajs/get-existing-redmine-issues ()
  "Parse `ajs/redmine-org-files', looking for entries with the `:redmine:' property.
Return a list of issue numbers from those properties."
  (let ((existing-redmine-ids nil))
    (mapcar (lambda (file)
	    (when (file-exists-p file)
	      (when-let ((parse-tree (ajs/parse-org-file file)))
		(org-element-map
		 parse-tree 'headline
		 (lambda (hl)
		   (when-let ((id (ajs/redmine-from-headline hl)))
			      (push id existing-redmine-ids)))))))
	    `(,ajs/redmine-inbox ,@ajs/redmine-org-files))
    existing-redmine-ids))

;;;; Utility
;; Convert an issue object into an org heading object. 
(defun ajs/redmine-to-headline (issue)
  "Convert an issue object into an org element headline."
  ;; ID is the first element of the issue object
  (let* ((id (nth 1 issue))
	 (title (plist-get issue :subject))
	 (title (concat title " :task:"))
	 (created
	  (format-time-string "<%Y-%m-%d %a>" (date-to-time (plist-get issue :created_on))))
	 (headline (org-element-create 'headline
				       `(:level 1 :title ,title)))
	 (section (org-element-create 'section))
	 (property-drawer (org-element-create 'property-drawer))
	 (id-property (org-element-create
		       'node-property
		       `(:key "REDMINE" :value ,id)))
	 (created-property (org-element-create
			    'node-property
			    `(:key "CREATED" :value ,created)))
	 (paragraph (org-element-create 'paragraph))
	 (description (plist-get issue :description))
	 (description (replace-regexp-in-string "\r" "" description))
	 (description (replace-regexp-in-string "^\\*" " *" description)))
    (add-to-list 'paragraph description t)
    (add-to-list 'property-drawer id-property t)
    (add-to-list 'property-drawer created-property t)
    (add-to-list 'section property-drawer t)
    (add-to-list 'headline section t)
    (add-to-list 'headline paragraph t)
    headline))
				   
;;;; Writing to file
;; Write a list of headlines to the given file.
;; Headlines are org element headline objects.
(defun ajs/insert-org-element-headlines (headlines file)
  "Insert the list of headline objects into the provided file as plaintext."
  (when (file-exists-p file)
	(find-file file)
	(mapcar (lambda (headline)
		  (let ((headline-string (org-element-interpret-data headline)))
		    (goto-char (point-max))
		    (insert
		     (concat headline-string "\n")))) headlines)))

;;;; Functionality
(defun ajs/get-redmine-issues ()
  "Get all issues assigned to me. No limit."
  (load-library ajs/secrets-file)
  (if (bound-and-true-p elmine/api-key)
      (elmine/get-issues :assigned_to_id "me" :limit 1000)
    (message "API key not loaded.")))

(defun check-redmine ()
  "1. Parse org files for existing redmine IDs
2. Make a request to redmine for all issues assigned to me
3. Find redmine issues with IDs not in my org files
4. Insert them into the redmine inbox org file"
  (interactive)
  (let* ((existing (ajs/get-existing-redmine-issues))
	 (issues (ajs/get-redmine-issues))
	 (headlines nil))
    (mapcar (lambda (issue)
	      ;; Org converts all properties to strings
	      (let ((id (number-to-string (nth 1 issue))))
		(unless (member id existing)
		  (push (ajs/redmine-to-headline issue) headlines)))) issues)
    (if headlines
	(progn
	  (ajs/insert-org-element-headlines headlines ajs/redmine-inbox)
	  (message "Inserted into redmine inbox."))
      (message "No new issues."))))

(provide 'ajs-redmine)
