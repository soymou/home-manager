{ pkgs, ... }:

{

  home.packages = with pkgs; [
    ripgrep
    fd
    clang
  ];

  services.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
  };
}
