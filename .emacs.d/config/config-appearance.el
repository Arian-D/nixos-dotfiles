;; Emacs appearance

;; Clean up the space
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)

;; My favorite font
(set-frame-font "Iosevka Nerd Font Mono-16:weight=light" nil t)

;; My favorite theme
(load-theme 'doom-challenger-deep t)

;; Display battery in the mode line
(display-battery-mode)

;; Set the beam style cursor
(set-default 'cursor-type 'bar)

;; Provide
(provide 'config-appearance)
