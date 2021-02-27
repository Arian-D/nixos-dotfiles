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
    (play-video youtube-uri)))

(defvar *my-keymap* (make-keymap "my-map"))
(define-key *my-keymap*
  "; ;" 'play-link
  "; b" 'switch-buffer
  "C-b" 'nyxt/web-mode:history-backwards)

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-scheme (keymap:make-scheme
		   scheme:vi-normal *my-keymap*))))

(define-configuration (buffer web-buffer)
  ((default-modes (append '(my-mode) %slot-default))))
