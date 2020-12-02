{pkgs ? import <nixpkgs>, ...}:

{
  programs.emacs = {
    enable = true;
    # package = pkgs.emacs26.override {
    #   imagemagick = pkgs.imagemagickBig;
    #   withXwidgets = true;
    # };
    extraPackages = epkgs: with epkgs; [
      # Essential
      helm                        # Survival
      evil                        # Trying it out for a while
      which-key                   # Saves you extra `C-h k'
      use-package                 # Declarative
      magit                       # Best Git client
      doom-themes                 # For chllenger deep, dracula, and other occasional themes
      doom-modeline               # hmmm
      challenger-deep-theme       # Cool theme; looks better than the doom one
      nix-mode                    # Nix
      nixos-options               # Might remove; doesn't seem to work
      company                     # Autocompletion
      direnv                      # Missing component of Nix
      elfeed                      # RSS, Atom, and YT
      telega                      # Telegram do be cool
      multi-term                  # export TERM=emacs
      projectile
      # Configs and stuff
      yaml-mode
      markdown-mode
      # Webshit garbage
      web-mode
      company-web
      # Java: for employability
      lsp-mode
      lsp-java
      # Haskal: for unemployability
      haskell-mode
      helm-hoogle
      # Coq... hehe
      proof-general
      company-coq
      # Python
      ein
      lsp-pyright
      # Uncommon Lisp
      slime
      slime-company
      geiser
      rainbow-delimiters
      paredit
      # Docker
      docker
      dockerfile-mode
      # Org: aimless attempt at getting my shit together
      org-roam                  # Still learning it
      org-alert
      org-evil
      # Miscx
      auctex                    # LaTeX to make Knuth proud
      slack                     # Slack AKA professional Discord
      pinentry                  # For GPG
      emojify
      gnu-apl-mode              # Don't ask me why
      key-chord
      use-package-chords
    ];
  };
}
