;;; core-project.el --- Project/file tree -*- lexical-binding: t; -*-

(use-package treemacs
  :commands (treemacs)
  :init (setq treemacs-width 32
              treemacs-show-hidden-files t))

(use-package treemacs-evil :after (treemacs evil))

(provide 'core-project)
;;; core-project.el ends here

