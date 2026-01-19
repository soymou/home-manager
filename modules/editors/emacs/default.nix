{ pkgs, ... }:

{
  home.packages = import ./pkgs/nix-packages.nix { inherit pkgs; };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages = epkgs: import ./pkgs/emacs-packages.nix { inherit epkgs pkgs; };
    extraConfig = builtins.concatStringsSep "\n" [
      "(add-to-list 'default-frame-alist '(undecorated . t))"
      (builtins.readFile ./config/core.el)
      (builtins.readFile ./config/evil-config.el)
      (builtins.readFile ./config/ui.el)
      (builtins.readFile ./config/completion.el)
      (builtins.readFile ./config/dev.el)
      (builtins.readFile ./config/lang.el)
      (builtins.readFile ./config/org-config.el)
      (builtins.readFile ./config/ai.el)
    ];

  };

  home.file.".emacs.d/snippets".source = ./snippets;
}
