;;   ____                 _____
;;  / ___|___ _____   _  | ____|_ __ ___   __ _  ___ ___
;; | |   / _ \_  / | | | |  _| | '_ ` _ \ / _` |/ __/ __|
;; | |__| (_) / /| |_| | | |___| | | | | | (_| | (__\__ \
;;  \____\___/___|\__, | |_____|_| |_| |_|\__,_|\___|___/
;;                |___/

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Elisp stuff
(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)

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

;; Move these somewhere else
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote auto))
 '(helm-completion-style (quote emacs)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
