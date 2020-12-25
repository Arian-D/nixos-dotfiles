;; -*- lexical-binding: t -*-
;;; Always have the server running
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; Use a separate file for custom behavior
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)	; Make sure it's there
  (load-file custom-file))

(let ((config-path (expand-file-name "config" user-emacs-directory)))
  ;; Add ./config/ folder to the `load-path'
  (add-to-list 'load-path config-path)
  ;; Load the configs
  (use-package config-behavior)
  (use-package config-haskell)
  (use-package config-org)
  (use-package config-appearance)
  (use-package config-nixos))
