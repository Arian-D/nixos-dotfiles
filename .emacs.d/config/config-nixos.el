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

(defun nix-collect-garbage (deep)	; TODO: Select depth (raw, old)
  "Collect nixos garbage"
  (interactive (list (yes-or-no-p "-d?")))
  (async-shell-command (concat "nix-collect-garbage" (if deep " -d" ""))))

;; TODO
(defun reboot ()
  "Reboot the machine (TODO)"
  (interactive))

(defun shutdown ()
  "Shutdown the machine (TODO)"
  (interactive))

(provide 'config-nixos)
