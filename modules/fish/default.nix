{ config, pkgs, ... }:

{
  # 1. THE FIX: Add your paths here. 
  # Since HM now manages the file, these will actually load!
  home.sessionPath = [
    "$HOME/.local/bin"
    "$HOME/go/bin"
    "$HOME/.cargo/bin"
    "$HOME/.config/emacs/bin"
  ];

  # 2. Enable Starship via Nix (replaces 'starship init fish | source')
  # This automatically injects the init script at the correct place.
  programs.starship.enable = true;

  home.packages = with pkgs; [
    eza
    neofetch
  ];

  programs.fish = {
    enable = true;

    # 3. Interactive Shell Init (Startup commands)
    # This runs only when you open a terminal, not for scripts.
    interactiveShellInit = ''
      set fish_greeting # Disable greeting
      neofetch          # Run neofetch
    '';

    # 4. Aliases
    shellAliases = {
      pamcan = "pacman";
      ls = "eza --icons";
      # Note: clear alias is usually unnecessary in Fish (Ctrl+L works), 
      # but here is how you define it if you want the explicit reset:
      clear = "printf '\\033[2J\\033[3J\\033[1;1H'"; 
      q = "qs -c ii";
    };

    # 5. Functions
    # Home Manager will write these into separate file blocks for cleaner loading.
    functions = {
      
      # Your custom prompt (Note: Starship might override this!)
      fish_prompt = {
        description = "Write out the prompt";
        body = ''
          printf '%s@%s %s%s%s > ' $USER $hostname \
            (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
        '';
      };

      # Your custom Update function
      update = {
        description = "System update utility";
        body = ''
          pushd ~/.config/home-manager

          switch $argv[1]
              # Case 1: 'update flake'
              case flake
                  nix flake update

              # Case 2: 'update home'
              case home
                  home-manager switch --flake .#mou

              # Case 3: 'update' (no arguments) -> Do both
              case ""
                  nix flake update
                  # The 'and' ensures we only rebuild if the update succeeded
                  and home-manager switch --flake .#mou

              # Fallback for typos
              case '*'
                  echo "Usage: update [flake|home]"
          end

          popd
        '';
      };
    };
  };
}
