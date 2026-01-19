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
    xwayland-satellite
  ];

  home.file = {
  };

  home.sessionVariables = {
    # EDITOR = "emacs";
    _JAVA_AWT_WM_NONREPARENTING = "1";
    NIXOS_OZONE_WL = "1";
    _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Djdk.gtk.version=3";
  };

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
    };
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
