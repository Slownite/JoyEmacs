;; ~/.config/joyemacs/init.el — you own this file (no Nix options required)
(setq inhibit-startup-message t
      use-package-always-ensure nil) ;; Nix provides packages

(eval-when-compile (require 'use-package))

;; Theme (doom-themes included by Nix)
(use-package doom-themes
  :config (load-theme 'doom-nord t))

;; Completion stack
(use-package vertico :init (vertico-mode 1))
(use-package orderless :init (setq completion-styles '(orderless basic)))
(use-package marginalia :init (marginalia-mode 1))
(use-package consult)
(use-package embark)
(use-package which-key :init (which-key-mode 1))

;; Evil (Vim keys)
(use-package evil
  :init (setq evil-want-keybinding nil
              evil-undo-system 'undo-redo)
  :config (evil-mode 1))
(use-package evil-collection :after evil :config (evil-collection-init))

;; Git
(use-package magit)

;; Your extras
(add-to-list 'load-path (expand-file-name "lisp" (file-name-directory load-file-name)))
;; (require 'my-bindings) ; put lisp/my-bindings.el and uncomment

