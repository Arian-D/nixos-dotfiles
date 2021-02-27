;;; Browser config

;;; The superior keybindings
(define-configuration (buffer web-buffer)
  ((default-modes (append '(vi-normal-mode) %slot-default))))

(defun play-video (url)
  "Play a video/stream using mpv"
  (uiop:run-program (list "mpv" url)))

(define-command play-link ()
  "Play a Link in mpv"
  (let ((youtube-uri
	  (nyxt/web-mode::query-hints "> mpv " 'url)))
    (play-youtube-video youtube-uri)))

