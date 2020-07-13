;; Org mode, AKA the greatest thing on the face of this planet

;; Most of my agenda/capture files go here
(setq org-directory "~/life")

;; Enable fill mode to disallow long lines
(add-hook 'org-mode-hook 'auto-fill-mode)
;; Org bullets for pretty headings
(add-hook 'org-mode-hook 'org-bullets-mode)

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq line-spacing 0.1)))

;; Cute lil rice
(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-agenda-block-separator " "
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)
      ;; Highlight LaTeX within org
      ;; org-highlight-latex-and-related 'latex)


;; Global org keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(setq org-default-notes-file
      (concat org-directory "/notes.org"))
(global-set-key "\C-cb" 'org-switchb)

;; Evaluate code blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((haskell . t)
   (python . t)
   (shell . t)))

(provide 'config-org)
