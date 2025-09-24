;;; core-completion.el --- Completion stack -*- lexical-binding: t; -*-

(use-package vertico :init (vertico-mode 1))
(use-package orderless :init (setq completion-styles '(orderless basic)))
(use-package marginalia :init (marginalia-mode 1))
(use-package consult)
(use-package embark)
(use-package which-key :init (which-key-mode 1))

(provide 'core-completion)
;;; core-completion.el ends here

