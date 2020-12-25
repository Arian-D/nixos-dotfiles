;; Keybindings and personal preferences
(use-package use-package-chords
  :config (key-chord-mode 1))

;; Shorten interactive yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package which-key
  :custom (which-key-idle-delay 0.1)
  :config
  (which-key-mode))

(use-package evil
  :custom (evil-flash-delay 1)
  :config
  (evil-mode 1)
  :bind
  (:map evil-normal-state-map
	("; b" . helm-buffers-list)
	("; q" . kill-buffer)
	("; s" . save-buffer)
	("; f" . helm-find-files)
	("; g" . keyboard-quit)
	("; TAB" . helm-M-x))
  :chords ("kj" . evil-normal-state))

(use-package magit
  :bind (("C-x g" . magit-status)
	 :map evil-normal-state-map
	 ("; G" . magit-status)))

(use-package slack
  :disabled
  :custom
  (slack-buffer-emojify t))

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
      )
    ;; Dev
    '(("https://www.tweag.io/rss.xml" tweag haskell dev)
      ("https://planetpython.org/rss20.xml" python general)
      )
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
  :hook
  (after-init . global-company-mode))

;;; LSP
;;; LaTeX
;; (use-package tex
;;   :defer t
;;   :ensure auctex
;;   :custom
;;   (TeX-auto-save t))

(provide 'config-behavior)
