;; Keybindings and personal preferences
(use-package use-package-chords
  :config (key-chord-mode 1))

;; Shorten interactive yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package helm
  :config (helm-mode 1) 		; Always start
  :custom (helm-ff-skip-boring-files t)	; Hide ugly files
  :bind 
  ("M-x" . helm-M-x)
  ("C-x r b" . helm-filtered-bookmarks)
  ("C-x C-f" . helm-find-files))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.2)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

(use-package evil
  :init (evil-mode 1)
  :custom (evil-flash-delay 1)
  :chords ("kj" . evil-normal-state))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package slack
  :config
  (setq slack-buffer-emojify t))

;; Elfeed
(use-package elfeed
  :bind ("C-x w" . elfeed)
  :custom
  (elfeed-feeds
   ;; Huge list of my favorite feeds; TODO: Move to a separate file
   (append
    '(("https://nullprogram.com/feed/" blog emacs general)
      ("https://news.ycombinator.com/rss" hn general)
      ("https://drewdevault.com/feed.xml" blog linux sway general)
      ("https://planetpython.org/rss20.xml" python general))
    ;; GNU slash Leenouxe
    '(("https://planet.kernel.org/rss20.xml" kernel linux)
      ("https://guix.gnu.org/feeds/blog.atom" guix linux)
      ("https://weekly.nixos.org/feeds/all.rss.xml" nixos linux)
      ("https://trisquel.info/en/node/feed" trisquel linux)
      ("https://planet.debian.org/atom.xml" debian linux)
      ("https://planet.ubuntu.com/atom.xml" ubuntu linux)
      ("https://security.gentoo.org/glsa/feed.rss" gentoo security linux)
      ("https://planet.gentoo.org/atom.xml" gentoo linux)
      ("https://fedoraplanet.org/atom.xml" fedora linux)
      ("https://www.archlinux.org/feeds/news/" arch linux))
    ;; Security; Im a hax0r b0i, apparently
    '(("https://forum.defcon.org/external?type=rss2&nodeid=19" def-con security)
      ("https://proofpoint.com/rss.xml" proof-point security)	  
      ("https://researchcenter.paloaltonetworks.com/unit42/feed" palo-alto security)
      ("https://reddit.com/r/netsec/.rss" netsec reddit security)
      ("https://feeds.feedburner.com/feedburner/Talos?format=xml" talos cisco security)
      ("https://hackaday.com/blog/feed/" hackaday blog security)
      ("https://feeds.trendmicro.com/Anti-MalwareBlog/" trend-micro security))
    ;; Quick mafs
    '(("https://golem.ph.utexas.edu/category/" n-category-cafe blog math))
    ;; Entertainment
    '(("https://xkcd.com/atom.xml" xkcd comics entertainment)))))

;;; Tex
(add-hook 'TeX-mode-hook
  (lambda ()
    (setq TeX-command-extra-options "-shell-escape")))

;;; Company
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;;; Development:

;;; LSP

;; Lisp & scheme
(use-package slime
  :custom (inferior-lisp-program "sbcl"))

(use-package paredit
  :hook
  ((lisp-mode . paredit-mode)
   (emacs-lisp-mode . paredit-mode)
   (scheme-mode . paredit-mode)))

(use-package rainbow-delimiters
  :hook
  ((lisp-mode . rainbow-delimiters-mode)
   (emacs-lisp-mode . rainbow-delimiters-mode)
   (scheme-mode . rainbow-delimiters-mode)))

;; Python
(use-package lsp-pyright
  :after lsp-mode)

;;; Nix
(use-package lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(provide 'config-behavior)
