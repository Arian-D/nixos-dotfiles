{pkgs ? import <nixpkgs>, ...}:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs26.override {
      imagemagick = pkgs.imagemagickBig;
      withXwidgets = true;
    };
    extraPackages = epkgs: with epkgs; [
      # Essential
      helm                        # Survival
      which-key                   # Saves you extra `C-h k'
      magit                       # Best Git client
      doom-themes                 # For chllenger deep, dracula, and other occasional themes
      doom-modeline               # hmmm
      challenger-deep-theme       # Cool theme; looks better than the doom one
      nix-mode                    # Nix
      nixos-options               # Might remove; doesn't seem to work
      company                     # Autocompletion
      direnv                      # For use with Lorri
      elfeed                      # RSS, Atom, and YT
      telega                      # Telegram do be cool
      multi-term
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
      elpy
      # Uncommon Lisp
      slime
      slime-company
      geiser
      rainbow-delimiters
      paredit
      # Docker
      docker                    # Doesn't seem to do much
      dockerfile-mode
      # Org: aimless attempt at getting my shit together
      org-roam                  # Still learning it
      org-bullets               # Might remove
      org-pomodoro              # Tic toc
      org-trello
      # Misc
      emms                      # Music
      emacsql                   # SQL client
      auctex                    # LaTeX to make Knuth proud
      slack                     # Slack AKA professional Discord
      pinentry                  # For GPG
      emojify
    ];
  };
}
