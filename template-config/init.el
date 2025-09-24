;;; init.el --- JoyEmacs entrypoint -*- lexical-binding: t; -*-

;; Basic hygiene
(setq inhibit-startup-message t
      use-package-always-ensure nil) ;; Nix provides packages
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(eval-when-compile (require 'use-package))

;; Add lisp/ to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load modules
(require 'core-ui)
(require 'core-evil)
(require 'core-completion)
(require 'core-git)
(require 'core-project)
(require 'core-leader)

(provide 'init)
;;; init.el ends here

