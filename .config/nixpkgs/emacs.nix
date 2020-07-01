{pkgs, ...}:

{
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      # Essential
      helm                        # Survival
      which-key
      magit                       # Git
      doom-themes                 # For chllenger deep
      doom-modeline
      challenger-deep-theme
      nix-mode                    # Nix
      nixos-options
      company                     # Autocompletion
      # Web
      web-mode
      company-web
      # Java
      lsp-mode
      lsp-java
      # Haskal
      haskell-mode
      helm-hoogle
      # Uncommon Lisp
      slime
      slime-company
      geiser
      rainbow-delimiters
      # Docker
      docker
      dockerfile-mode
      # Org
      org-bullets
      org-pomodoro
      org-trello
      # Misc
      emms                      # Music
      telega                    # Telegram
      emacsql                   # SQL client
      auctex
    ];
  };
}
