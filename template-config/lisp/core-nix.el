;;; core-nix.el --- Nix language plumbing (TS, LSP, format) -*- lexical-binding: t; -*-
;;; Commentary:
;; - Prefers Tree-sitter (nix-ts-mode) if available
;; - LSP via Eglot with the 'nil' server
;; - Format on save: eglot-format-buffer -> alejandra -> nixfmt -> nixpkgs-fmt
;; - Eldoc and Flymake integration

;;; Code:

;; Associate .nix files and prefer TS major mode if installed
(add-to-list 'auto-mode-alist
             (cons "\\.nix\\'" (if (fboundp 'nix-ts-mode) 'nix-ts-mode 'nix-mode)))

(when (boundp 'major-mode-remap-alist)
  (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode)))

;; ---------- Formatting ----------
(defgroup core-nix nil
  "Nix language configuration."
  :group 'languages)

(defcustom core/nix-formatters '("alejandra" "nixfmt" "nixpkgs-fmt")
  "External Nix formatters to try, in order, when Eglot is unavailable."
  :type '(repeat string)
  :group 'core-nix)

(defun core/nix--external-formatter ()
  "Return the first available external formatter executable for Nix, or nil."
  (seq-find #'executable-find core/nix-formatters))

(defun core/nix-format-buffer ()
  "Format current buffer.
Prefer Eglot's server formatting; otherwise use an external formatter if found."
  (interactive)
  (cond
   ;; Eglot formatting (if attached and server supports it)
   ((and (boundp 'eglot-managed-mode) eglot-managed-mode
         (fboundp 'eglot-format-buffer))
    (eglot-format-buffer))
   ;; External formatter fallback
   ((let ((fmt (core/nix--external-formatter)))
      (when fmt
        (let* ((orig-point (point))
               (buf (current-buffer))
               (tmp (generate-new-buffer " *nixfmt*"))
               (status (call-process-region (point-min) (point-max) fmt
                                            nil tmp nil)))
          (unwind-protect
              (if (and (numberp status) (zerop status))
                  (progn
                    (with-current-buffer tmp
                      (let ((formatted (buffer-string)))
                        (with-current-buffer buf
                          (erase-buffer)
                          (insert formatted)
                          (goto-char orig-point))))
                    (message "Formatted with %s" fmt))
                (progn
                  (message "Formatter %s failed (status %s). See *nixfmt* buffer." fmt status)
                  (display-buffer tmp)))
            (unless (get-buffer-window tmp) (kill-buffer tmp))))))
    )
   (t (message "No formatter available (Eglot not active; no external formatter found)."))))

(defun core/nix--format-on-save ()
  "Buffer-local hook to format on save."
  (add-hook 'before-save-hook #'core/nix-format-buffer nil t))

;; ---------- Eglot (LSP) ----------
(with-eval-after-load 'eglot
  ;; Tell Eglot to use 'nil' for Nix buffers
  (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nil"))))

(defun core/nix--eglot-setup ()
  "Ensure Eglot is running if available; enable Eldoc & Flymake."
  (when (featurep 'eglot)
    ;; Start/attach Eglot (no error if server missing; it will just echo a message)
    (eglot-ensure))
  ;; Eldoc is generally on by default; ensure it's enabled.
  (when (fboundp 'eldoc-mode) (eldoc-mode 1))
  ;; Flymake cooperates with Eglot; turn it on for diagnostics.
  (when (fboundp 'flymake-mode) (flymake-mode 1)))

(defun core/nix-setup ()
  "Main setup for Nix buffers."
  (core/nix--eglot-setup)
  (core/nix--format-on-save)
  ;; A couple of sensible defaults
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2))

(add-hook 'nix-mode-hook     #'core/nix-setup)
(add-hook 'nix-ts-mode-hook  #'core/nix-setup)

;; Optional: a convenient keybinding for manual format
;; Change to your leader system if you have one.
(with-eval-after-load 'nix-mode
  (define-key (current-global-map) (kbd "C-c =") #'core/nix-format-buffer))

(provide 'core-nix)
;;; core-nix.el ends here
