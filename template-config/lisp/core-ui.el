;; core-ui.el --- UI/UX for JoyEmacs -*- lexical-binding: t; -*-

(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-nord t)

  ;; Explicitly load extensions when autoloads aren’t available
  (ignore-errors (require 'doom-themes-ext-visual-bell))
  (when (fboundp 'doom-themes-visual-bell-config)
    (doom-themes-visual-bell-config))

  ;; (Optional) other extensions if you use them:
  (ignore-errors (require 'doom-themes-ext-org))
  (when (fboundp 'doom-themes-org-config)
    (doom-themes-org-config)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 28
        doom-modeline-icon t))

(use-package all-the-icons :defer t)

(use-package dashboard
  :init
  ;; Make dashboard the first thing you see
  (setq inhibit-startup-screen t
        initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-items '((recents  . 8)
                          (projects . 5)))
  (dashboard-setup-startup-hook))

(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(pixel-scroll-precision-mode 1)

(provide 'core-ui)
;;; core-ui.el ends here
