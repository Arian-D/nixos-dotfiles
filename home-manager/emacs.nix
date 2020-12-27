{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      # Essential
      helm                        # Survival
      helm-descbinds
      evil                        # Superior keybindings
      evil-collection             # For other places
      which-key                   # Saves you extra `C-h k'
      use-package                 # Declarative-ish
      magit                       # Best Git client
      doom-themes                 # For chllenger deep, dracula, and other occasional themes
      doom-modeline               # hmmm
      challenger-deep-theme       # Cool theme; looks better than the doom one
      poet-theme                  # For org and md
      nix-mode                    # Nix
      company                     # Autocompletion
      direnv                      # Missing component of Nix
      envrc                       # For direnv projects
      elfeed                      # RSS, Atom, and YT
      telega                      # Telegram do be cool
      multi-term                  # export TERM=emacs
      vterm
      yasnippet
      # LSP stuff
      lsp-mode
      lsp-ui
      # Configs and stuff
      yaml-mode
      markdown-mode
      # Webshit garbage
      web-mode
      company-web
      # Java: for employability
      lsp-java
      # Haskal: for unemployability
      haskell-mode
      lsp-haskell
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
      org-gcal
      org-super-agenda
      org-alert
      org-bullets
      org-evil
      # Miscx
      # auctex                    # LaTeX to make Knuth proud
      slack                     # Slack AKA professional Discord
      pinentry                  # For GPG
      emojify
      gnu-apl-mode              # Don't ask me why
      key-chord
      use-package-chords
    ];
  };
}
