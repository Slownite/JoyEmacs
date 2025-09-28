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
(setq inhibit-startup-screen t)

;; Show Dashboard on startup (reliably)
(setq inhibit-startup-screen t)

(use-package dashboard
  :ensure t
  :demand t
  :init
  
  (setq dashboard-banner-directory user-emacs-directory 
	dashboard-startup-banner (expand-file-name "M-x_butterfly.png" user-emacs-directory)
        dashboard-center-content t
        dashboard-items '((recents . 8) (projects . 5))
        ;; Hand Emacs a live buffer immediately so it doesn't fall back to *scratch*
        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  ;; Belt-and-suspenders: if something else stole focus, open the dashboard anyway
  (add-hook 'emacs-startup-hook #'dashboard-open))



(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(pixel-scroll-precision-mode 1)

(provide 'core-ui)
;;; core-ui.el ends here
