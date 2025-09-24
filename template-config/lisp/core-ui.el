;;; core-ui.el --- UI/UX for JoyEmacs -*- lexical-binding: t; -*-

(use-package doom-themes
  :config
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 28
        doom-modeline-icon t))

(use-package all-the-icons :defer t)
(use-package dashboard
  :config
  (setq dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-items '((recents  . 8)
                          (projects . 5)))
  (dashboard-setup-startup-hook))

(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(pixel-scroll-precision-mode 1)

(provide 'core-ui)
;;; core-ui.el ends here

