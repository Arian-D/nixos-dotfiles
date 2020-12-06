;; Emacs appearance

;; Clean up the space
(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package menu-bar
  :config
  (menu-bar-mode -1))

(use-package scroll-bar
  :config
  (set-scroll-bar-mode nil))

;; Remove the black borders after resize
(setq frame-resize-pixelwise t)

;; My favorite font
(set-frame-font "Fantasque Sans Mono-16:weight=light" nil t)

;; Pwetty icons for doom-modeline
(use-package all-the-icons)

;; Modeline
(use-package doom-modeline
  :after all-the-icons
  :hook (after-init . doom-modeline-mode))

;; My favorite theme
(use-package doom-themes
  :config
  (load-theme 'doom-city-lights t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Display battery in the mode line
(use-package battery
  :config
  (display-battery-mode 1))

;; Display time
(use-package time
  :config
  (display-time-mode 1))

;; Set the beam style cursor
(set-default 'cursor-type 'bar)

;; Line numbers everywhere
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))
;; Provide
(provide 'config-appearance)
