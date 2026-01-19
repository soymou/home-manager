{ pkgs, ... }:

{
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      source ${./config/config.fish}
    '';
  };

  home.packages = with pkgs; [
    eza 
    neofetch
  ];

  xdg.configFile."fish/functions".source = ./config/functions;
  xdg.configFile."fish/completions".source = ./config/completions;
  xdg.configFile."fish/conf.d".source = ./config/conf.d;
  xdg.configFile."fish/auto-Hypr.fish".source = ./config/auto-Hypr.fish;
  xdg.configFile."fish/fish_variables".source = ./config/fish_variables;
}
