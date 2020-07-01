;;     _         _             _       _____
;;    / \   _ __(_) __ _ _ __ ( )___  | ____|_ __ ___   __ _  ___ ___
;;   / _ \ | '__| |/ _` | '_ \|// __| |  _| | '_ ` _ \ / _` |/ __/ __|
;;  / ___ \| |  | | (_| | | | | \__ \ | |___| | | | | | (_| | (__\__ \
;; /_/   \_\_|  |_|\__,_|_| |_| |___/ |_____|_| |_| |_|\__,_|\___|___/

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Always have the server running
;; (unless (server-running-p)
;;   (server-start))

;; Use a separate file for custom behavior
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)	; Make sure it's there
    (shell-command (concat "touch " custom-file)))
(load-file custom-file)

;; SLIME
(setq inferior-lisp-program "sbcl")

;; Global company
(add-hook 'after-init-hook 'global-company-mode)

;; Add ./config/ folder to the `load-path'
(let ((config-path (expand-file-name "config" user-emacs-directory)))
  (add-to-list 'load-path config-path))

;; Load the configs
(mapc 'require '(config-helm
		 config-haskell
		 config-org
		 config-appearance
		 config-behavior
		 config-nixos))

;; Put these in a separate file
(defun nmap (host)
  (interactive "sHost: ")
  (async-shell-command (concat "nmap -Pn " host)))

(defun nmap-service-scan (host)
  (interactive "sHost: ")
  (async-shell-command (concat "nmap -Pn -sV " host)))
