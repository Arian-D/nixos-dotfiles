;; NixOS-related helpful functions

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
  (async-shell-command "home-manager switch"))

(defun nix-collect-garbage (deep)
  "Collect nixos garbage"
  (interactive (list (yes-or-no-p "-d?")))
  (async-shell-command (concat "nix-collect-garbage" (if deep " -d" ""))))

(defun reboot (sure)
  "Reboot the machine"
  (interactive (list (yes-or-no-p "Are you sure?")))
  (if sure (shell-command "reboot")))

(defun shutdown (sure)
  "Shutdown the machine"
  (interactive (list (yes-or-no-p "Are you sure?")))
  (if sure (let ((default-directory "/sudo::"))
	     (shell-command "shutdown"))))

(provide 'config-nixos)
