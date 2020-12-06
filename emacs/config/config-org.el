;; Org mode, AKA the greatest thing on the face of this planet

(use-package org
  :custom
  (org-directory "~/me")
  ;;; Cute lil rice
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-default-notes-file (concat org-directory "/notes.org"))
  ;;; Code blocks
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  ;;; org-agenda config
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  :init
  :config
  ;;; Enable fill mode to disallow long lines
  (add-hook 'org-mode-hook 'auto-fill-mode)
  ;;; Make it look nice
  (add-hook 'org-mode-hook 'prettify-symbols-mode)
  ;;; Evaluate code blocks
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t)
     (scheme . t)
     (python . t)
     (shell . t)))
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-switchb)))

;;; Pwetty bullets
(use-package org-bullets
  :after org)

;;; EVIL
(use-package org-evil
  :after org)

;;; Roam
(use-package org-roam
  :after org
  :custom
  (org-roam-directory "~/roam")
  (org-roam-link-title-format "(→%s)")
  (org-roam-completion-system 'helm)
  :config
  (add-hook 'after-init-hook 'org-roam-mode)
  :bind (("C-M-g" . org-roam-find-file)
	 :map org-roam-mode-map
	 ("C-c i" . org-roam-insert)))

(provide 'config-org)
