;;; -*- lexical-binding: t -*-

(defvar-local ajs/modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        "Narrow"))
  "Mode line construct to report the narrowed state of the current buffer.")

(put 'ajs/modeline-narrow 'risky-local-variable t)

(defun ajs/buffer-text ()
  "Get propertized text for the buffer section of the modeline"
  (let* ((file-name (buffer-file-name))
	 (pre-text (if (and file-name (project-current nil))
		       (concat (project-name (project-current nil)) "--")
		     ""))
	 (remote-symbol (if (and file-name (file-remote-p default-directory))
			    "@" ""))
	 (text (if file-name
		   (let ((short-file-name (file-name-nondirectory file-name)))
		     (concat " " (nerd-icons-icon-for-file short-file-name) " " short-file-name))
		 (buffer-name))))
    (propertize (concat remote-symbol pre-text text)
		'face 'italic)))

(defvar-local ajs/buffer-name
    '(:eval
      (ajs/buffer-text)))

(put 'ajs/buffer-name 'risky-local-variable t)

(defvar-local ajs/read-only-buffer
    '(:eval
      (let ((any-read-only (or buffer-read-only buffer-file-read-only)))
	(if (and any-read-only (mode-line-window-selected-p))
	    (propertize (concat
			 "["
			 (if buffer-read-only "B" "")
			 (if buffer-file-read-only "F" "")
			 "]")
			'face 'error)
	  ""))))

(defvar-local ajs/file-properties
    '(:eval
      (when (mode-line-window-selected-p)
	(concat
	 (propertize "B" 'face (if buffer-read-only 'error 'success))
	 (when buffer-file-name
	   (propertize "F" 'face (if buffer-file-read-only 'error 'success)))
	 (when (buffer-modified-p) (propertize "*" 'face 'warning))))))

(put 'ajs/file-properties 'risky-local-variable t)

(put 'ajs/read-only-buffer 'risky-local-variable t)

(defun ajs/get-major-mode ()
  "Get propertized major mode text for modeline"
  (let ((text (symbol-name major-mode)))
    (propertize
     (concat
      "("
      text
      ")"))))

(defvar-local ajs/major-mode-text
    '(:eval
      (if (mode-line-window-selected-p)
	  (ajs/get-major-mode)
	"")))

(put 'ajs/major-mode-text 'risky-local-variable t)

(defun ajs/set-modeline ()
  "Sets the modeline variable, call after theme setup."
  (setq-default mode-line-format
		'("%e"
		  ajs/modeline-narrow
		  " "
		  ajs/file-properties
		  " "
		  ajs/buffer-name
		  " "
		  ajs/major-mode-text
		  " "
		  )))

(ajs/set-modeline)

(provide 'ajs-modeline)
