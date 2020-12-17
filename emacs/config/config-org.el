;; Org mode, AKA the greatest thing on the face of this planet

(use-package org
  :after evil
  :ensure nil
  :defer nil
  :custom
  (org-directory "~/me")
  ;;; Cute lil rice
  (org-startup-indented t)
  (org-hidden-keywords '(title author email date))
  (org-highlight-latex-and-related '(native))
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-hide-block-startup t)
  (org-hide-macro-markers t)
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-default-notes-file (concat org-directory "/notes.org"))
  ;;; Code blocks
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-confirm-babel-evaluate nil)
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
  (evil-define-key 'normal org-mode-map (kbd "; c '") 'org-edit-special)
  (evil-define-key 'normal org-mode-map (kbd "; l") 'org-insert-link)
  (evil-define-key 'normal org-mode-map (kbd "; t") 'org-todo)
  (evil-define-key 'normal org-mode-map (kbd "; s s") 'org-schedule)
  (evil-define-key 'normal org-mode-map (kbd "; s d") 'org-deadline)
  (evil-define-key 'normal org-mode-map (kbd "; SPC") 'org-ctrl-c-ctrl-c)
  (evil-define-key 'normal org-mode-map (kbd "M-l") 'org-shiftright)
  (evil-define-key 'normal org-mode-map (kbd "M-h") 'org-shiftright)
  (evil-define-key 'normal org-mode-map (kbd "M-k") 'org-shiftup)
  (evil-define-key 'normal org-mode-map (kbd "M-j") 'org-shiftdown)
  (evil-define-key 'normal org-mode-map (kbd "M-K") 'org-metaup)
  (evil-define-key 'normal org-mode-map (kbd "M-J") 'org-metadown)
  (evil-define-key '(normal visual) 'global
    (kbd "; a") 'org-agenda)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-switchb)))

(setq initial-major-mode 'org-mode
      initial-scratch-message "#+TITLE: Scratchpad")

(use-package org-agenda
  :after org
  :defer nil
  :ensure nil
  :custom
  (org-agenda-block-separator "")
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
(use-package org-gcal
  :after org
  :defer t
  :config
  (let* ((gcal-creds-file (expand-file-name "gcal-creds.el.gpg" user-emacs-directory))
	 (creds-file-exists (file-exists-p gcal-creds-file)))
    (when creds-file-exists
      (load gcal-creds-file)
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
