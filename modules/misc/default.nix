{ config, pkgs, inputs, ... }:
{

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.packages = with pkgs; [
    discord
    spotify
    gemini-cli
    vscode
    neovim
    protonvpn-gui
    zathura
    tor
  ]; 
}
