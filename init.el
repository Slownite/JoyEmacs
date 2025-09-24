;; init.el — minimal, extensible
(setq inhibit-startup-message t)
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)

;; use-package de MELPA via Nix -> pas besoin de bootstrap internet
(eval-when-compile (require 'use-package))

;; UX de base
(use-package which-key :config (which-key-mode 1))
(use-package vertico :config (vertico-mode 1))
(use-package orderless
  :init (setq completion-styles '(orderless basic)))
(use-package marginalia :config (marginalia-mode 1))
(use-package consult)
(use-package embark)

;; Git
(use-package magit :commands (magit-status))

;; LSP simple avec Eglot (serveurs fournis par Nix/devShell)
(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (bash-ts-mode . eglot-ensure)))

;; Tree-sitter automatique (Emacs 29)
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; Thème (inclus par Nix)
(load-theme 'modus-vivendi t)

