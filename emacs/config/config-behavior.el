;; Keybindings and personal preferences

;;; Evil mode
(setq evil-flash-delay 1)
(evil-mode 1)
;; Shorten interactive yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Which-key mode
(setq which-key-idle-delay 0.2)
(which-key-setup-side-window-right-bottom)
(which-key-mode)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; (defmacro safe-load (file-path &optional create-if-absent)
;;   "Load an `.el' file if it exists"
;;   (if (file-exists-p file-path)
;;        `(load ,file-path)
;;      (if create-if-absent
;; 	  (shell-command (concat "touch " file-path)))))

;; Slack
(setq slack-buffer-emojify t)

;; Youtube, to be used with elfeed and interactively
(defvar video-player-command "cvlc ")
(defvar youtube-url "https://invidio.us/watch?raw=1&v=") ; Replace with YT if Invidious goes down
(defun watch-youtube (url-or-id)
  "Watch the given youtube video"
  (interactive "sURL: ")
  (let ((url (if (string-prefix-p "http" url-or-id) ; TODO: Replace http with https (cond)
		 url-or-id (concat youtube-url url-or-id))))
    (call-process-shell-command ;TODO: Print errors
     (format "%s '%s'" video-player-command url)
     nil 0)))


;; Elfeed
(require 'elfeed)
(global-set-key (kbd "C-x w") 'elfeed)
(defun elfeed-modify-feed (feed apply-to-url &optional tag)
  "Modify elfeed url and tags"
  (let* ((fst (car feed))
	 (url (funcall apply-to-url fst))
	 (tags (cdr feed))
	 (feed (if tag (cons tag tags) tags)))
    (cons url feed)))	  
;; Huge list of my favorite feeds; TODO: Abstract extra mapcars
(setq elfeed-feeds
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
       '(("https://golem.ph.utexas.edu/category/" n-category-cafe blog math)
	 )
       ;; Entertainment
       '(("https://xkcd.com/atom.xml" xkcd comics entertainment))
       ;; YouTube channels
       (mapcar
	(lambda (feed)
	  (elfeed-modify-feed
	   feed
	   (lambda (id)
 	     (concat 
	      "https://www.youtube.com/feeds/videos.xml?channel_id="
	      id))
	   'youtube))
	'(("UCYO_jab_esuFRV4b17AJtAw" 3b1b math)
	  ("egEraZP9yXQ" vsauce general science)
	  ("UC9-y-6csu5WGm29I7JiwpnA" computerphile general)))))

(defun elfeed-watch ()
  "Watch the selected entry"
  (interactive)
  (mapc
   (lambda (entry)
     (let* ((id (elfeed-entry-id entry))
	    (youtube-ids (cdr id))
	    (youtube-id (caddr (split-string youtube-ids ":")))
	    (url (concat "https://invidio.us/watch?raw=1&v=" youtube-id)))
       (watch-youtube url)))
   (elfeed-search-selected)))
(define-key elfeed-search-mode-map (kbd "w") 'elfeed-watch)

;;; Tex
(add-hook 'TeX-mode-hook
  (lambda ()
    (setq TeX-command-extra-options "-shell-escape")))

;; Company
;; (setq company-idle-delay 0)
(add-hook 'after-init-hook 'global-company-mode)

;;; Development:

;; Mode
(add-to-list 'auto-mode-alist
	     '("\\.gs\\'" . javascript-mode) ; Icky Google script
	     '("\\.pl\\'" . prolog-mode))

;;; LSP
(require 'lsp-mode)
(require 'lsp-pyright)

;;; Java

;; Lisp & scheme
(setq inferior-lisp-program "sbcl")
;; Hooks
;;; TODO: Change to mapc (3×2)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

;; Python

;;; Nix
(add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                  :major-modes '(nix-mode)
                  :server-id 'nix))

(provide 'config-behavior)
