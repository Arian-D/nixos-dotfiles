;; Personal functions

;; Networking
(defun nmap (host)
  "Nmap a host"
  (interactive "sHost: ")
  (async-shell-command (concat "nmap -Pn " host)))

(defun nmap-service-scan (host)
  "Nmap a host and its services"
  (interactive "sHost: ")
  (async-shell-command (concat "nmap -Pn -sV " host)))

(defun whos-there (host)
  "Check who is logged in on a remote host"
  (interactive "sWhere? ")
  (let ((default-directory (concat "/ssh:" host ":")))
    (async-shell-command "who -u")))

;; NixOS
(defun nixos-rebuild-switch (upgrade)
  "Build and switch to the new NixOS config"
  (interactive (list (yes-or-no-p "--upgrade?")))
  (save-some-buffers)
  (let ((default-directory "/sudo::"))
    (async-shell-command (concat "nixos-rebuild switch"
				 (if upgrade " --upgrade" "")))))

(defun home-manager-switch ()
  "Build and switch the new home-manager config"
  (interactive)
  (save-buffer)
  (async-shell-command "home-manager switch"))

(defun nix-collect-garbage (deep)
  "Collect nixos garbage"
  (interactive (list (yes-or-no-p "-d?")))
  (async-shell-command (concat "nix-collect-garbage" (if deep " -d" ""))))

;; System
(defun shutdown (sure &optional reboot)
  "Shutdown the machine"
  (interactive (list (yes-or-no-p "Are you sure?")))
  (if sure (let ((default-directory "/sudo::"))
	     (shell-command (if reboot "reboot" "shutdown")))))

(defun reboot (sure)
  "Reboot the machine"
  (interactive (list (yes-or-no-p "Are you sure?")))
  (shutdown sure t))

;; Nixpecifc
(defun nix-setup nil nil)

;; TODO: Dev setup: `default.nix' creator, `nix-options' with helm, and `nix-env' crap

(provide 'config-nixos)

