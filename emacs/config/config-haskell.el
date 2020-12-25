;; (use-package haskell-mode
;;   :hook
;;   ;; Declaration manager (imenu, C-M-* for navigation, etc)
;;   (haskell-mode . haskell-decl-scan-mode)
;;   ;; Make it interactive
;;   (haskell-mode . interactive-haskell-mode)
;;   ;; Create a template for haskell modules
;;   (haskell-mode . haskell-auto-insert-module-template)
;;   ;; :bind
;;   ;; (:map ("M-." . haskell-mode-jump-to-def-or-tag))
;;   :custom
;;   (haskell-font-lock-symbols t)	; Cool symbols
;;   (haskell-process-suggest-remove-import-lines t)
;;   (haskell-process-auto-import-loaded-modules t)
;;   (haskell-process-log t)
;;   ;; Cabal, stack, or ghci
;;   (haskell-process-type 'auto))

(provide 'config-haskell)
