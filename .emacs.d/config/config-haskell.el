(require 'haskell-mode)

;; Use nix-shell instead of the wacky cabal/stack
;; (setq haskell-process-wrapper-function
;;       (lambda (argv)
;;         (list "nix-shell"
;;               "-I"
;;               "."
;;               "--command"
;;               (mapconcat 'identity argv " "))))

;; Startup hook for haskell mode
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends)
		 (append '((company-capf company-dabbrev-code))
			 company-backends))))

;; Declaration manager (imenu, C-M-* for navigation, etc)
(add-hook 'haskell-mode-hook
	  'haskell-decl-scan-mode)

;; Make it interactive
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (interactive-haskell-mode)))

;; Create a template for haskell modules
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

;; Create a TAGS file every time you C-x C-s
(setq haskell-tags-non-save t
      haskell-font-lock-symbols t	; Cool symbols
      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t
      ;; Cabal, stack, or ghci
      haskell-process-type 'auto)

;; Use GHCi to find def, or use tags file
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
(provide 'config-haskell)
