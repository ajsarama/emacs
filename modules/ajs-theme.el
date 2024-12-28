;;; Modus Themes
(use-package modus-themes
  :demand t
  :bind (("<f5>" . modus-themes-toggle))
  :config
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
	modus-themes-mixed-fonts t
	modus-themes-prompts '(bold)
	modus-themes-completions '((matches . (bold))
				   (selection . (italic)))
	modus-themes-headings '((0 . (variable-pitch 1.0 light))
				(1 . (variable-pitch 1.4))
				(2 . (variable-pitch 1.2))
				(t . (variable-pitch 1.0))))) 

(modus-themes-select 'modus-operandi-tinted)

(provide 'ajs-theme)
