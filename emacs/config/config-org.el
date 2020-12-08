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
  (org-confirm-babel-evaluate nil)
  ;;; Enable fill mode to disallow long lines
  :hook
  ((org-mode . auto-fill-mode)
   (org-mode . prettify-symbols-mode))
  :config
  ;;; Evaluate code blocks
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t)
     (scheme . t)
     (emacs-lisp . t)
     (python . t)
     (shell . t)))
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-switchb)))

(setq initial-major-mode 'org-mode
      initial-scratch-message "#+TITLE: Scratchpad")

(use-package org-agenda
  :after org
  :custom
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled))

;;; Pwetty bullets
(use-package org-bullets
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

;;; EVIL
(use-package org-evil
  :after org)

;;; Google calendar
(let* ((gcal-creds-file (expand-file-name "gcal-creds.el.gpg" user-emacs-directory))
       (creds-file-exists (file-exists-p gcal-creds-file)))
  (when creds-file-exists
    (load gcal-creds-file)
    (use-package org-gcal
      :after org
      :config
      (setq org-gcal-client-id my/gcal-client-id
	    org-gcal-client-secret my/gcal-client-secret
	    org-gcal-fetch-file-alist '(("arianxdehghani@gmail.com" .  "~/me/calendar.org"))))))

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
