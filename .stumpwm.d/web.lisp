(in-package :stumpwm)
(defvar *duckduckgo-url* "https://start.duckduckgo.com/?kae=d")

(defcommand search-youtube (q)
    ((:string "youtube> "))
  (run-shell-command
   (firefox
    (format nil "https://www.youtube.com/results?search_query=~a" q))))
    
(define-interactive-keymap search-web ()
  ((kbd "y") "search-youtube"))

(defun choose-website (website)
  (cond ((equal website "canvas") "canvas.csun.edu")
	((equal website "github") "github.com")
	((equal website "gmail") "gmail.com")
	((equal website "wordnik") "wordnik.com")		 
	((equal website "csun") "auth.csun.edu")
	((equal website "lisp") "www.gigamonkeys.com/book")
	((equal website "hackernews") "news.ycombinator.com")
	((equal website "zybooks") "https://learn.zybooks.com/signin")
	((equal website "discord") "https://discordapp.com/login")
	(t website)))

(defmacro firefox (url)
  `(format nil "exec firefox '~a'" ,url))

(defmacro duckduckgo-search (query)
  `(format nil "~a&q=~a" *duckduckgo-url* ,query))

(defcommand open-website (website)
    ((:string "Website> "))
  (run-shell-command (firefox (choose-website website))))

(defcommand search-query (query)
    ((:string "Search> "))
  (run-shell-command (firefox (duckduckgo-search query))))
