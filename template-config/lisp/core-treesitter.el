;;; core-treesitter.el --- Tree-sitter setup -*- lexical-binding: t; -*-

;; Emacs 29+ has native Tree-sitter (treesit). This module:
;; - Declares language sources (for easy one-shot install)
;; - Remaps classic major modes to *-ts-mode
;; - Adds a helper to install all grammars

(defgroup core-treesitter nil
  "Tree-sitter config."
  :group 'languages)

;; If your grammars come from your OS/Nix, point Emacs to them:
;; Example Nix paths (uncomment the one that matches your system):
(let ((dirs (getenv "JOYEMACS_TS_DIRS")))
  (when dirs
    (dolist (d (split-string dirs ":" t))
      (when (file-directory-p d)
        (add-to-list 'treesit-extra-load-path d)))))

(add-to-list 'treesit-extra-load-path "/run/current-system/sw/lib/tree-sitter") ; NixOS system profile
(add-to-list 'treesit-extra-load-path (expand-file-name "~/.nix-profile/lib/tree-sitter")) ; user profile
(dolist (dir '("~/.nix-profile/lib/tree-sitter"
               "~/.local/state/nix/profiles/default/lib/tree-sitter"
               "~/.local/state/nix/profiles/home-manager/lib/tree-sitter"
               "~/.local/state/nix/profiles/home-manager/home-path/lib/tree-sitter"))
  (let ((d (expand-file-name dir)))
    (when (file-directory-p d)
      (add-to-list 'treesit-extra-load-path d))))
;; Highest level of font-lock (more syntax categories)
(setq treesit-font-lock-level 4)

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))

  ;; Tell Emacs where to fetch grammars if you want to build them with M-x treesit-install-language-grammar
  (setq treesit-language-source-alist
        '((bash        . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c           . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp         . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (cmake       . ("https://github.com/uyha/tree-sitter-cmake"))
          (css         . ("https://github.com/tree-sitter/tree-sitter-css"))
          (go          . ("https://github.com/tree-sitter/tree-sitter-go"))
          (html        . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript  . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json        . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua         . ("https://github.com/Azganoth/tree-sitter-lua"))
          (markdown    . ("https://github.com/ikatyang/tree-sitter-markdown"))
          (python      . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust        . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml        . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx         . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx"))
          (typescript  . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript"))
          (yaml        . ("https://github.com/ikatyang/tree-sitter-yaml"))
	  (nix         . ("https://github.com/nix-community/tree-sitter-nix"))))

  ;; Prefer Tree-sitter major modes when available
  (setq major-mode-remap-alist
        '((bash-mode          . bash-ts-mode)
          (c-mode             . c-ts-mode)
          (c++-mode           . c++-ts-mode)
          (cmake-mode         . cmake-ts-mode)
          (css-mode           . css-ts-mode)
          (js-mode            . js-ts-mode)
          (json-mode          . json-ts-mode)
          (lua-mode           . lua-ts-mode)
          (python-mode        . python-ts-mode)
          (rust-mode          . rust-ts-mode)
          (sh-mode            . bash-ts-mode)
          (toml-mode          . toml-ts-mode)
          (typescript-mode    . typescript-ts-mode)
          (yaml-mode          . yaml-ts-mode)
	  (nix-mode           . nix-ts-mode)))

  ;; File associations for TS/TSX/MD
  (add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'"  . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

  ;; Handy command: install all grammars we declared above.
  (defun core/treesit-install-all-grammars ()
    "Install all grammars from `treesit-language-source-alist`."
    (interactive)
    (when (yes-or-no-p "Install all Tree-sitter grammars now? ")
      (dolist (lang (mapcar #'car treesit-language-source-alist))
        (condition-case err
            (progn
              (message "Installing %s…" lang)
              (treesit-install-language-grammar lang))
          (error (message "Failed installing %s: %s" lang err))))
      (message "Done."))))

(provide 'core-treesitter)
;;; core-treesitter.el ends here
