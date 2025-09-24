;;; init.el --- JoyEmacs entrypoint -*- lexical-binding: t; -*-

(setq inhibit-startup-message t
      use-package-always-ensure nil)   ;; Nix provides packages
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
(eval-when-compile (require 'use-package))

;; --- Auto-load every .el in lisp/ (recursively) ----------------------------
(let* ((joy-dir (file-name-as-directory
                 (or user-emacs-directory
                     (expand-file-name "~/.config/joyemacs/"))))
       (lisp-dir (expand-file-name "lisp" joy-dir)))

  ;; Add lisp/ and all its subdirs to load-path
  (defun joy/add-subdirs-to-load-path (dir)
    (when (file-directory-p dir)
      (add-to-list 'load-path dir)
      (dolist (f (directory-files dir t "^[^.]" t))
        (when (file-directory-p f)
          (joy/add-subdirs-to-load-path f)))))
  (joy/add-subdirs-to-load-path lisp-dir)

  ;; Load every *.el found
  (when (file-directory-p lisp-dir)
    (dolist (file (directory-files-recursively lisp-dir "\\.el\\'"))
      ;; Load quietly; errors will show in *Messages* if any
      (load file nil 'nomessage))))

;;; Put your *minimal* bootstrap below (theme, a couple of essentials), or
;;; keep it empty and let files in lisp/ do everything.

;; Example: a tiny baseline so UI isn’t blank if lisp/ is empty
(use-package doom-themes :config (load-theme 'doom-nord t))
(use-package which-key :init (which-key-mode 1))

