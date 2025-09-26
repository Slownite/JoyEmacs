{
  description = "JoyEmacs — nixvim-style install, Elisp-configured Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        epkgs = pkgs.emacsPackagesFor pkgs.emacs;

        joyEmacs = epkgs.emacsWithPackages (_:
          with epkgs; [
            use-package
            vertico
            orderless
            marginalia
            consult
            embark
            embark-consult
            which-key
            evil
            evil-collection
            doom-themes
            magit
            doom-modeline
            all-the-icons # icons in modeline/treemacs/dashboard
            dashboard # start screen (Doom-like)
            treemacs
            treemacs-evil # project/file tree
            centaur-tabs # (optional) tabs like Doom
            rainbow-delimiters # rainbow parens
            hl-todo # TODO/FIXME highlights
            general # leader keys (Doom-style)
            nix-ts-mode
            nix-mode
            eglot
          ]);

        defaultCfgDir = "$HOME/.config/joyemacs";

        # ⬇️ creates a package with bin/joyemacs
        launcher = pkgs.writeShellScriptBin "joyemacs" ''
          set -euo pipefail
          CFG_DIR="''${JOYEMACS_HOME:-${defaultCfgDir}}"
          mkdir -p "$CFG_DIR"
          if [ ! -f "$CFG_DIR/init.el" ]; then
            echo "JoyEmacs: No $CFG_DIR/init.el found."
            echo "Run: nix run github:Slownite/JoyEmacs#install"
          fi
          exec ${joyEmacs}/bin/emacs -Q --load "$CFG_DIR/init.el" "$@"
        '';

        installer = pkgs.writeShellScriptBin "joyemacs-install" ''
          set -euo pipefail
          CFG_DIR="''${JOYEMACS_HOME:-${defaultCfgDir}}"
          if [ -e "$CFG_DIR" ] && [ "$(ls -A "$CFG_DIR" 2>/dev/null | wc -l)" -gt 0 ]; then
            echo "JoyEmacs: $CFG_DIR is not empty; refusing to overwrite."
            echo "Move it or set JOYEMACS_HOME to a new path."
            exit 1
          fi
          mkdir -p "$CFG_DIR"
          cp -r ${./template-config}/* "$CFG_DIR"/
          echo "JoyEmacs: Starter config written to $CFG_DIR"
          echo "Now run: nix run github:Slownite/JoyEmacs#joyemacs"
        '';
      in {
        packages.default = joyEmacs;

        # 👇 program must be a STRING path
        apps.joyemacs = {
          type = "app";
          program = "${launcher}/bin/joyemacs";
        };
        apps.install = {
          type = "app";
          program = "${installer}/bin/joyemacs-install";
        };
        apps.default = {
          type = "app";
          program = "${launcher}/bin/joyemacs";
        };

        devShells.default =
          pkgs.mkShell { packages = with pkgs; [ 
          joyEmacs 
          ripgrep 
          fd 
          git
    tree-sitter-grammars.tree-sitter-nix   # ← add this
    nil alejandra 
          ]; };
      });
}
