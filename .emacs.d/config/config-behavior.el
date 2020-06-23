;; Keybindings and personal preferences

;; Use print as M-x
(global-set-key (kbd "<print>") #'helm-M-x)

;; Which-key mode
;; (setq which-key-idle-delay 10000)
;; (setq which-key-idle-secondary-delay 0.05)
(which-key-setup-side-window-right-bottom)
(which-key-mode)

;; AUCTeX
;; TODO

;; ERC
(load "~/.emacs.d/.erc-auth")

;; Company
(setq company-idle-delay 0)

;; Shorten interactive yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'config-behavior)
