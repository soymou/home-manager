{ pkgs, ... }:

{

  home.packages = with pkgs; [
    ripgrep
    fd
    clang
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
  };
}
