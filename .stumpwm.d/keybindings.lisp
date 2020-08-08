(in-package :stumpwm)

(load "~/.stumpwm.d/web.lisp")

(defmacro execute-within-terminal (list)
  `(format nil "exec alacritty --command ~{~a~^ ~}" ,list))

(defcommand open-website (website)
    ((:string ">>> "))
  (run-shell-command (firefox (choose-website website))))

(defmacro keybinding (key command)
  `(define-key *root-map* (kbd ,key) ,command))

(defmacro keybinding-top (key command)
  `(define-key *top-map* (kbd ,key) ,command))

(defmacro create-keybindings (keys-and-commands)
  `(loop for (key command) in ,keys-and-commands
      do (keybinding key command)))

;;; Personal
(create-keybindings
 '(("T" "toggle-gaps")
  ("c" "exec alacritty")
  ("C-c" "exec alacritty")
  ("M-f" "exec firefox")
  ("M-c" "exec alacritty -e calcurse -q")
  ("z" "exec zathura")
  ("M-r" "exec rofi -show drun")
  ("M-w" "open-website")
  ("M-s" "search-query")
  ("M-S" "search-web")))

;;; Volume
(keybinding-top "XF86AudioMute" "exec amixer -M set Master +1 toggle")
(keybinding-top "XF86AudioLowerVolume" "exec amixer -M set Master 5%-")
(keybinding-top "XF86AudioRaiseVolume" "exec amixer -M set Master 5%+")

;;; TODO: Add brightness keys
;;(keybinding-top "" "exec amixer -M set Master 5%+")
