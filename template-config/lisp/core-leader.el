;;; core-leader.el --- Doom-style leader key -*- lexical-binding: t; -*-

(use-package general
  :config
  (general-create-definer my/leader
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my/leader
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save")

    "b"  '(:ignore t :which-key "buffers")
    "bb" '(switch-to-buffer :which-key "switch")
    "bd" '(kill-this-buffer :which-key "kill")

    "w"  '(:ignore t :which-key "windows")
    "wv" '(split-window-right :which-key "vsplit")
    "ws" '(split-window-below :which-key "hsplit")
    "wd" '(delete-window :which-key "delete")

    "p"  '(:ignore t :which-key "project")
    "pt" '(treemacs :which-key "treemacs")

    "g"  '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "status")

    "/"  '(consult-ripgrep :which-key "ripgrep")))

(provide 'core-leader)
;;; core-leader.el ends here

