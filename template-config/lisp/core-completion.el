;;; core-completion.el --- Completion stack -*- lexical-binding: t; -*-

(use-package vertico
  :demand t
  :config
  (vertico-mode 1))

(use-package marginalia
  :after vertico
  :demand t
  :config
  (marginalia-mode 1))

(use-package orderless :init (setq completion-styles '(orderless basic)))
(use-package consult)
(use-package embark)
(use-package which-key :init (which-key-mode 1))

;; Corfu (in-buffer completion popup)
(use-package corfu
  :ensure nil                 ;; you’re using Nix; set to t if using package.el
  :demand t                   ;; load now so the mode function exists
  :init
  ;; Tune BEFORE loading
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-cycle t
        corfu-preselect 'prompt)
  :config
  ;; Enable AFTER it’s loaded (fixes “void: global-corfu-mode”)
  (global-corfu-mode 1)
  ;; Keybindings
  (define-key corfu-map (kbd "TAB")     #'corfu-next)
  (define-key corfu-map (kbd "<tab>")   #'corfu-next)
  (define-key corfu-map (kbd "<backtab>") #'corfu-previous) ;; Shift-Tab
  (define-key corfu-map (kbd "RET")     #'corfu-insert)
  (define-key corfu-map (kbd "<return>") #'corfu-insert))
;; ------ TAB insert a litteral tab when cordu is not active -----
;; Global binding (Corfu's map takes precedence when the popup is active).
 (define-key global-map (kbd "TAB") #'indent-for-tab-command)
;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))
(provide 'core-completion)
;;; core-completion.el ends here

