{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    home-manager = {
    	url = "github:nix-community/home-manager";
        inputs.nixpkgs.follows = "nixpkgs";
    };

    zen-browser = {
	url = "github:0xc000022070/zen-browser-flake";
	inputs = {
	  nixpkgs.follows = "nixpkgs";
	      home-manager.follows = "home-manager";
	};
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    
    nvchad = {
      url = "github:soymou/nvchad";
      flake = false;
    };

    nix4nvchad = {
      url = "github:nix-community/nix4nvchad";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nvchad-starter.follows = "nvchad";
    };

  };

  outputs = { self, nixpkgs, home-manager,  ... }@inputs: 
  {
    templest = import ./dev-shells;

    homeConfigurations."mou" = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs { system = "x86_64-linux"; config.allowUnfree = true; };
      extraSpecialArgs = { inherit inputs;};
      modules = [
        {
	  nixpkgs.overlays = [
	    inputs.emacs-overlay.overlays.default
	  ];
	}
      	./home.nix
      ];
    };
  };
}
