;;; Garbage Collection
; Garbage collection threshold, note default is 800,000.
(defvar ajs/gc-cons-threshold (* 1000 1000 8))

; Disable garbage collection during minibuffer usage
(defun ajs/disable-gc ()
(setq gc-cons-threshold most-positive-fixnum))

(defun ajs/enable-gc ()
  (setq gc-cons-threshold ajs/gc-cons-threshold))
(ajs/enable-gc)

(add-hook 'minibuffer-setup-hook #'ajs/disable-gc)
(add-hook 'minibuffer-exit-hook #'ajs/enable-gc)

;;; Bookmarks
(setq bookmark-fringe-mark nil)

;;; Backups and Lockfiles
(setq make-backup-files nil)
(setq backup-inhibited nil)
; Lockfiles prevent other sources from editing our open files.
(setq create-lockfiles nil)

;;; Customize
;; Send the config changes to the void.
(setq custom-file (make-temp-file "emacs-custom-"))

;;; Enabled and Disabled Commands
;; Enable these
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(narrow-to-region narrow-to-page upcase-region downcase-region))
;; And disable these
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(iconify-frame diary))

;;; Packages
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 2)
        ("melpa" . 1)))

;; Always install packages when declared in this config
(setq use-package-always-ensure t)

;; Allow custom themes without warnings
(setq custom-safe-themes t)

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

;; Sample:
;; (ajs/emacs-keybind global-map
;;   "C-z" nil
;;   "C-x b" #'switch-to-buffer
;;   "C-x C-c" nil
;; ;; Notice the -map as I am binding keymap here, not a command:
;;   "C-c b" beframe-prefix-map
;;   "C-x k" #'kill-buffer)


;;; Modules
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("lisp" "modules" "lisp/elmine"))

(require 'ajs-basics)
(require 'ajs-visuals)
(require 'ajs-org)
(require 'ajs-modeline)
(require 'ajs-edit)
(require 'ajs-redmine)
(require 'ajs-minibuffer)
