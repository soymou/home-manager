function fish_prompt -d "Write out the prompt"
    # This shows up as USER@HOST /home/user/ >, with the directory colored
    # $USER and $hostname are set by fish, so you can just use them
    # instead of using `whoami` and `hostname`
    printf '%s@%s %s%s%s > ' $USER $hostname \
        (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
end

if status is-interactive # Commands to run in interactive sessions can go here
    # No greeting
    set fish_greeting

    # Use starship
    starship init fish | source

    neofetch

    # Aliases
    alias pamcan pacman
    alias ls 'eza --icons'
    alias clear "printf '\033[2J\033[3J\033[1;1H'"
    alias q 'qs -c ii'
end


function update
    pushd ~/.config/home-manager

    switch $argv[1]
        # Case 1: 'update flake'
        case flake
            nix flake update

        # Case 2: 'update nixos'
        case home
            home-manager switch --flake .#mou

        # Case 3: 'update' (no arguments) -> Do both
        case ''
            nix flake update
            # The 'and' ensures we only rebuild if the update succeeded
            and home-manager switch --flake .#mou

        # Fallback for typos
        case '*'
            echo "Usage: update [flake|home]"
    end

    popd
end

