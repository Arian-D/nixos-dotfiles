;;; Browser config


;;; Search engines
(define-configuration buffer
    ((search-engines
      (list
       (make-instance 'search-engine
		      :shortcut "word"
		      :search-url "https://www.wordnik.com/words/~a"
		      :fallback-url "https://www.wordnik.com")
       (make-instance 'search-engine
		      :shortcut "gh"
		      :search-url "https://github.com/search?q=~a"
		      :fallback-url "https://github.com/trending")
       (make-instance 'search-engine
		      :shortcut "yc"
		      :search-url "https://hn.algolia.com/?dateRange=all&page=0&prefix=true&sort=byPopularity&type=story&query=~a"
		      :fallback-url "https://news.ycombinator.com")
       (make-instance 'search-engine
		      :shortcut "ddg"
		      :search-url "https://duckduckgo.com/?kae=d&q=~a"
		      :fallback-url "https://duckduckgo.com/")))))

;;; Link stuff
(defun play-video (url)
  "Play a video/stream using mpv"
  (let ((url 
	  (cond ((stringp url) url)
		((quri:uri-p url) (quri:render-uri url)))))
    (uiop:run-program (list "mpv" url))))

(define-command play-link ()
  "Play a Link in mpv"
  (let ((youtube-uri
	  (url
	   (nyxt/web-mode::query-hints "> mpv " 'car))))
    (play-video youtube-uri)))

(define-command intelligent-follow ()
  "Best action based on the link (WIP)"
  (let ((uri
	  (url
	   (nyxt/web-mode::query-hints ">>> " 'car))))
    (play-video uri)))


(defvar *my-keymap* (make-keymap "my-map"))
(define-key *my-keymap*
    "; ;" 'intelligent-follow
    "; b" 'switch-buffer
    "backspace" 'nyxt/web-mode:scroll-page-up ; Make it similar to info
    "C-b" 'nyxt/web-mode:history-backwards)

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-scheme (keymap:make-scheme
		   scheme:vi-normal *my-keymap*))))

(defvar *reddit-url* "https://old.reddit.com")
(define-command visit-subreddit ()
  "Visit a subreddit"
  (let* ((input (prompt :prompt "test "
			:sources (make-instance 'prompter:raw-source)))
	 (subreddit (car input))
	 (uri (format nil "~a/r/~a" *reddit-url* subreddit))
	 (quri-uri (quri:uri uri)))
    (make-buffer-focus :url quri-uri)
    ))

;;; The superior keybindings
(define-configuration web-buffer
    ((default-modes (append '(vi-normal-mode my-mode) %slot-default%))))
