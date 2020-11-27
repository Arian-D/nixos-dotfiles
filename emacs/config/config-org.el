;; Org mode, AKA the greatest thing on the face of this planet

;; Most of my org files go here
(setq org-directory "~/me")

;; Enable fill mode to disallow long lines
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Make it look nice
(add-hook 'org-mode-hook 'prettify-symbols-mode)
(add-hook 'org-mode-hook 'emojify-mode)

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
   (scheme . t)
   (python . t)
   (shell . t)))

;; Roam
(require 'org-roam)
(setq org-roam-directory "~/roam"
      org-roam-link-title-format "(→%s)"
      org-roam-completion-system 'helm)
(add-hook 'after-init-hook 'org-roam-mode)
(global-set-key "\C-\M-g" 'org-roam-find-file)
(define-key org-roam-mode-map (kbd "C-c i") 'org-roam-insert)


(provide 'config-org)
