;; Emacs appearance

;; Clean up the space
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)

;; My favorite font
(set-frame-font "Fantasque Sans Mono: style=Thin-18" nil t)

;; My favorite theme
(load-theme 'doom-dracula t)

;; Display battery in the mode line
(display-battery-mode)

;; Set the beam style cursor
(set-default 'cursor-type 'bar)

;; Line number mode for my favorite modes
(mapc (lambda (mode-hook)
	  (add-hook mode-hook 'display-line-numbers-mode))
      '(emacs-lisp-mode-hook
	lisp-mode-hook
	TeX-mode-hook
	LaTeX-mode-hook
	nix-mode-hook
	haskell-mode-hook
	c-mode-hook))

;; Provide
(provide 'config-appearance)
