(in-package :stumpwm)
(load "~/.stumpwm.d/keybindings.lisp")

(defcommand set-wallpaper (f)
    ((:string "Path> "))
  (run-shell-command (format nil "feh --bg-fill ~a" f)))

(set-wallpaper "~/wallpaper")
;;; TODO: Add volume, brightness

;; live editing

;; A wacky hack to get the swank file on Nix
(defvar swank-file
  (concat
   (run-shell-command "nix eval --raw nixpkgs.lispPackages.swank" t)
   "/lib/common-lisp/swank/swank-loader.lisp"))
(load swank-file)
(swank-loader:init)
(let ((server-running nil))
  (defcommand
      swank () ()
    "Toggle the swank server on/off"
    (if server-running
	(progn
	  (swank:stop-server 4005)
	  (echo-string
	   (current-screen)
	   "Stopping swank.")
	  (setf server-running nil))
	(progn
	  (swank:create-server :port 4005
			       :style swank:*communication-style*
			       :dont-close t)
	  (echo-string
	   (current-screen)
	   "Starting swank...")
	  (setf server-running t)))))

; #C678DD
(setf *mode-line-foreground-color* "#C678DD")

(defcommand ip () ()
  (run-shell-command "exec curl ifconfig.me" t))

(defcommand reboot (yes-or-no)
    ((:string "Are you sure? "))
  (if yes-or-no
      (run-shell-command "reboot" t)))

(set-font "Fantasque Sans Mono")
(toggle-mode-line (current-screen)
		  (current-head))

;; Mode line
(setf *screen-mode-line-format*
      (list "%n %W |^> %C | %M | %B | %d"))

;;; External modules
(set-module-dir "/home/someone/.stumpwm.d/modules")
(load-module "battery-portable")
(load-module "cpu")
(load-module "mem")
(load-module "swm-gaps")

;;; Gaps
(setf swm-gaps:*head-gaps-size* 0)
(setf swm-gaps:*inner-gaps-size* 10)
(setf smw-gaps:*outer-gaps-size* 40)
