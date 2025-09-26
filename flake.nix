{
  description =
    "JoyEmacs — reproducible Emacs with Tree-sitter + LSP (nix run friendly)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        epkgs = pkgs.emacsPackagesFor pkgs.emacs;

        # ---- Emacs with your packages ----
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
            general
            hl-todo
            rainbow-delimiters
            doom-themes
            doom-modeline
            all-the-icons
            dashboard
            treemacs
            treemacs-evil
            centaur-tabs
            magit
            nix-ts-mode
            eglot
            corfu
          ]);

        # ---- LSP servers + formatters ----
        servers = with pkgs; [
          nil
          #python
          basedpyright # or pyright
          ruff
          black
          #js
          vue-language-server
          biome
          typescript-language-server
          #cpp
          llvmPackages_21.clang-tools
        ];

        defaultCfgDir = "$HOME/.config/joyemacs";

        # ---- Launcher ----
        launcher = pkgs.writeShellApplication {
          name = "joyemacs";
          runtimeInputs = [ joyEmacs ] ++ servers;
          text = ''
            set -euo pipefail
            CFG_DIR="''${JOYEMACS_HOME:-${defaultCfgDir}}"
            exec ${joyEmacs}/bin/emacs -Q --load "$CFG_DIR/init.el" "$@"
          '';
        };

        #---- Installer ----
        installer = pkgs.writeShellApplication {
          name = "joyemacs-install";
          text = ''
            set -euo pipefail
            CFG_DIR="''${JOYEMACS_HOME:-$HOME/.config/joyemacs}"
            if [ -e "$CFG_DIR/init.el" ]; then
              echo "JoyEmacs: $CFG_DIR/init.el already exists; nothing to do."
              exit 0
            fi
            mkdir -p "$CFG_DIR"
            if [ -d "${./template-config}" ]; then
              cp -r ${./template-config}/* "$CFG_DIR"/
              echo "JoyEmacs: copied template-config to $CFG_DIR"
            else
              echo "JoyEmacs: no template-config in repo."
              echo "Create $CFG_DIR/init.el and any lisp/ modules you want."
            fi
          '';
        };

      in {
        packages.default = joyEmacs;
        apps.install = {
          type = "app";
          program = "${installer}/bin/joyemacs-install";
        };

        apps.joyemacs = {
          type = "app";
          program = "${launcher}/bin/joyemacs";
        };
        apps.default = {
          type = "app";
          program = "${launcher}/bin/joyemacs";
        };

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [ joyEmacs ] ++ servers ++ [ ripgrep fd git ];
        };
      });
}
