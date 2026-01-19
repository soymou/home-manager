{ config, pkgs, inputs, ... }:

{
  imports = [
    ./modules/browsers/zen-browser 
    ./modules/editors/emacs
    ./modules/misc
    ./modules/custom/minecraft
    ./modules/fish
    ./modules/starship
  ];

  home.username = "mou";
  home.homeDirectory = "/home/mou";

  home.stateVersion = "25.11";

  home.packages = with pkgs; [
    neovim
  ];

  home.file = {
  };

  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  programs.git = {
    enable = true;
    settings = {
      user.email = "emilio.junoy@gmail.com";
      user.name = "soymou";
    };
  };

  programs.home-manager.enable = true;
}
