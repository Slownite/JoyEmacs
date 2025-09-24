{
   description = "The joy emacs distribution";
   inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  # Optionnel mais recommandé pour versionner aussi ta home
        home-manager.url = "github:nix-community/home-manager/release-25.05";
        home-manager.inputs.nixpkgs.follows = "nixpkgs";
   };
   outputs = {self, nixpkgs, home-manager, ...}:
    let
     system = "x86_64-linux";
     pkgs = import nixpkgs { inherit system;};
     emacsBase = pkgs.emacs;
     emacsWith = (pkgs.emacsPackagesFor emacsBase).emacsWithPackages;
     joyEmacs = emacsWith (epkgs: with epkgs; [
        use-package
        magit
        vertico
        consult
        orderless
        marginalia
        embark
        which-key
        eglot
        treesit-auto
        modus-themes
        evil
        evil-collection
        evil-surround
        evil-commentary
        doom-themes
      ]);
    in
    {
        packages.${system}.emacs = joyEmacs;
        devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
                  myEmacs
                  ripgrep fd
                  git
                  # LSP servers (exemples)
                  nodejs
                  nodePackages_latest.pyright
                  bash-language-server
                  # tree-sitter grammars (utile avec treesit-auto)
                  tree-sitter
                ];
        };
    };

}
