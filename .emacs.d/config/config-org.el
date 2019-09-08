;; Org mode, AKA the greatest thing on the face of this planet

;; Enable fill mode to disallow long lines
(add-hook 'org-mode-hook 'auto-fill-mode)


(add-hook 'org-mode-hook 'org-bullets-mode)

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq line-spacing 0.1)))

(setq org-startup-indented t
      ;; Replace the "..." with an arrow
      org-ellipsis " "
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)
      ;; Highlight LaTeX within org
      ;; org-highlight-latex-and-related 'latex)


;; Global org-agenda key
(global-set-key "\C-ca" 'org-agenda)

(provide 'config-org)
