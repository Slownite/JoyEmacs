;;; core-python.el --- Python (TS, LSP, format, venv) -*- lexical-binding: t; -*-
;;; Commentary:
;; - Uses python-ts-mode if available (Tree-sitter)
;; - Eglot LSP with basedpyright / pyright / pylsp (auto-pick)
;; - Format-on-save (eglot if supported → ruff/black/yapf fallback)
;; - Auto-detect .venv in project and prefer it

;;; Code:

(require 'seq)
(require 'project)

;; ------------- Modes -------------
;; If your core-treesitter already remaps python-mode → python-ts-mode,
;; this is mostly redundant, but it doesn't hurt to be explicit.
(when (fboundp 'python-ts-mode)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

;; ------------- Per-project venv detection -------------
(defun core/python--project-root ()
  (when-let* ((pr (project-current nil)))
    (car (project-roots pr))))

(defun core/python--venv-path ()
  "Return path to a project-local venv if found (e.g., .venv)."
  (when-let* ((root (core/python--project-root)))
    (let ((candidates (list
                       (expand-file-name ".venv" root)
                       (expand-file-name ".direnv/python" root))))
      (seq-find (lambda (p) (file-exists-p (expand-file-name "bin/python" p)))
                candidates))))

(defun core/python--activate-venv-envvars ()
  "Prefer project .venv/bin on exec-path & PATH for this buffer."
  (when-let* ((venv (core/python--venv-path))
              (bin  (expand-file-name "bin" venv)))
    ;; PATH/environment (buffer-local equivalents)
    (make-local-variable 'exec-path)
    (setq exec-path (cons bin (remove bin exec-path)))
    (make-local-variable 'process-environment)
    (setenv "VIRTUAL_ENV" venv)
    (setenv "PATH" (mapconcat #'identity (cons bin (parse-colon-path (or (getenv "PATH") ""))) ":"))
    ;; Python shell/interpreter
    (setq-local python-shell-virtualenv-root venv)
    (setq-local python-shell-interpreter (expand-file-name "python" bin))))

;; ------------- Eglot (LSP) -------------
;; Auto-pick the first available server: basedpyright → pyright → pylsp.
(defun core/python--pick-eglot-server ()
  (or (and (executable-find "basedpyright-langserver") '("basedpyright-langserver"))
      (and (executable-find "pyright-langserver")      '("pyright-langserver" "--stdio"))
      (and (executable-find "pylsp")                   '("pylsp"))))

(with-eval-after-load 'eglot
  ;; We set a buffer-local entry just before eglot-ensure; but having a sane default helps.
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))))

(defun core/python--eglot-setup ()
  "Start Eglot with an available server and enable Eldoc/Flymake."
  (when (featurep 'eglot)
    (when-let* ((cmd (core/python--pick-eglot-server)))
      ;; Prepend a buffer-local mapping so this buffer uses the chosen server.
      (setq-local eglot-server-programs
                  (cons `((python-mode python-ts-mode) . ,cmd)
                        (remove (assoc '(python-mode python-ts-mode) eglot-server-programs)
                                eglot-server-programs))))
    (unless (core/python--pick-eglot-server)
      (message "core-python: no Python LSP server found (install basedpyright or pyright or pylsp)"))
    (eglot-ensure))
  (when (fboundp 'eldoc-mode)  (eldoc-mode 1))
  (when (fboundp 'flymake-mode) (flymake-mode 1)))

;; ------------- Formatting -------------
(defgroup core-python nil "Python config." :group 'languages)

(defcustom core/python-formatters
  ;; ruff format → black → yapf
  '((:cmd "ruff"  :args ("format" "--stdin-filename" "%FILE%" "-"))
    (:cmd "black" :args ("-"))
    (:cmd "yapf"  :args ()))
  "Formatters to try when Eglot formatting isn’t available.
Each entry is plist with :cmd and :args. %FILE% gets replaced with buffer-file-name or a dummy."
  :type '(repeat plist)
  :group 'core-python)

(defun core/python--call-formatter (cmd args)
  "Run external formatter CMD with ARGS on the current buffer via stdin → stdout."
  (let* ((tmp (generate-new-buffer " *pyfmt*"))
         (status (apply #'call-process-region (point-min) (point-max) cmd
                        nil tmp nil
                        (mapcar (lambda (a)
                                  (if (string= a "%FILE%")
                                      (or buffer-file-name "stdin.py")
                                    a))
                                args))))
    (unwind-protect
        (if (and (numberp status) (zerop status))
            (let ((pt (point)))
              (erase-buffer)
              (insert (with-current-buffer tmp (buffer-string)))
              (goto-char pt)
              (message "Formatted with %s" cmd)
              t)
          (progn
            (message "Formatter %s failed (status %s). See *pyfmt* buffer." cmd status)
            (display-buffer tmp) nil))
      (unless (get-buffer-window tmp) (kill-buffer tmp)))))

(defun core/python-format-buffer ()
  "Format current Python buffer. Prefer Eglot; else fall back to external tools."
  (interactive)
  (cond
   ;; If server supports formatting, let Eglot do it.
   ((and (boundp 'eglot-managed-mode) eglot-managed-mode
         (fboundp 'eglot-format-buffer))
    (eglot-format-buffer))
   ;; Try external formatters in order.
   (t
    (let ((done nil))
      (dolist (fmt core/python-formatters)
        (unless done
          (let ((cmd (plist-get fmt :cmd))
                (args (plist-get fmt :args)))
            (when (executable-find cmd)
              (setq done (core/python--call-formatter cmd args))))))
      (unless done
        (message "No formatter available (install ruff or black or yapf)."))))))

;; Optional: organize imports using ruff (if available).
(defun core/python-organize-imports ()
  "Organize imports using ruff (check --fix with selection I)."
  (interactive)
  (if (executable-find "ruff")
      ;; Use ruff to apply import fixes; prefer stdin so we don't touch disk.
      (let* ((tmp (generate-new-buffer " *pyimports*"))
             (status (call-process-region
                      (point-min) (point-max) "ruff"
                      nil tmp nil
                      "check" "--select" "I" "--fix" "--silent"
                      "--stdin-filename" (or buffer-file-name "stdin.py") "-")))
        (unwind-protect
            (if (and (numberp status) (zerop status))
                (let ((pt (point)))
                  (erase-buffer)
                  (insert (with-current-buffer tmp (buffer-string)))
                  (goto-char pt)
                  (message "Organized imports with ruff"))
              (progn
                (message "ruff import fix failed (status %s). See *pyimports* buffer." status)
                (display-buffer tmp)))
          (unless (get-buffer-window tmp) (kill-buffer tmp))))
    (message "ruff not found; install it to organize imports.")))

(defun core/python--format-on-save ()
  (add-hook 'before-save-hook #'core/python-format-buffer nil t))

;; ------------- Main setup -------------
(defun core/python-setup ()
  "Main setup for Python buffers."
  (core/python--activate-venv-envvars)
  (core/python--eglot-setup)
  (core/python--format-on-save)
  (setq-local indent-tabs-mode nil
              tab-width 4
              python-indent-offset 4))

(add-hook 'python-mode-hook    #'core/python-setup)
(add-hook 'python-ts-mode-hook #'core/python-setup)

;; Handy keybindings (change to fit your leader system)
(global-set-key (kbd "C-c C-f") #'core/python-format-buffer)
(global-set-key (kbd "C-c C-o") #'core/python-organize-imports)

(provide 'core-python)
;;; core-python.el ends here
