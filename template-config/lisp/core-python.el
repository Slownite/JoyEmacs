;;; core-python.el --- Python (TS, LSP, format, venv) -*- lexical-binding: t; -*-
;;; Commentary:
;; - Uses python-ts-mode if available (Tree-sitter)
;; - Eglot LSP with basedpyright / pyright / pylsp (auto-pick)
;; - Format-on-save (eglot if supported → ruff/black/yapf fallback)
;; - Auto-detect .venv in project and prefer it

;;; Code:
;;; core-python.el --- Python via Eglot/TS -*- lexical-binding: t; -*-

;; Use tree-sitter mode when available
(with-eval-after-load 'treesit
  (when (treesit-language-available-p 'python)
    (add-to-list 'major-mode-remap-alist
                 '(python-mode . python-ts-mode))))

;; Start Eglot for Python buffers
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)

;; Prefer project .venv automatically if present
(defun je/python-activate-project-venv ()
  (when-let* ((root (locate-dominating-file default-directory ".venv"))
              (venv (expand-file-name ".venv" root))
              ((file-directory-p venv)))
    (setenv "VIRTUAL_ENV" venv)
    (setq python-shell-virtualenv-root venv
          python-shell-interpreter (expand-file-name "bin/python" venv))))
(add-hook 'python-mode-hook #'je/python-activate-project-venv)
(add-hook 'python-ts-mode-hook #'je/python-activate-project-venv)

(provide 'core-python)
;;; core-python.el ends here
