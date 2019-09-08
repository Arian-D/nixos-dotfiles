(in-package :stumpwm)
(load "~/.stumpwm.d/keybindings.lisp")
(set-module-dir "/home/someone/.stumpwm.d/contrib")
(load-module "battery-portable")
(load-module "cpu")
(load-module "mem")
(load-module "swm-gaps")

;; Gaps
(setf swm-gaps:*head-gaps-size* 5)
(setf swm-gaps:*inner-gaps-size* 10)
(setf swm-gaps:*outer-gaps-size* 10)

(defcommand wal (p)
    ((:string "Path> "))
  (run-shell-command (format nil "wal -i ~a" p)))
(wal "~/Pictures/wallpaper/neo-outrun.jpg")
;;; TODO: Add volume, brightness

;; live editing
;; (defvar swank-file "/home/someone/.emacs.d/elpa/slime-20200414.1444/swank-loader.lisp")

;; (load swank-file)
;; (swank-loader:init)
;; (let ((server-running nil))
;;   (defcommand swank () ()
;; 	      "Toggle the swank server on/off"
;; 	      (if server-running
;; 		  (progn
;; 		    (swank:stop-server 4005)
;; 		    (echo-string
;; 		     (current-screen)
;; 		     "Stopping swank.")
;; 		    (setf server-running nil))
;; 		  (progn
;; 		    (swank:create-server :port 4005
;; 					 :style swank:*communication-style*
;; 					 :dont-close t)
;; 		    (echo-string
;; 		     (current-screen)
;; 		     "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm).")
;; 		    (setf server-running t)))))

; #C678DD
(setf *mode-line-foreground-color* "#C678DD")

(defcommand ip () ()
  (run-shell-command "exec curl ifconfig.me" t))

(defcommand reboot (yes-or-no)
    ((:string "Are you sure? "))
  (if yes-or-no
      (run-shell-command "reboot" t)))

(set-font "Hack Nerd Font 20")
(toggle-mode-line (current-screen)
		  (current-head))

;; Mode line
(setf *screen-mode-line-format*
      (list "%W |^> %C | %M | %B | %d"))
