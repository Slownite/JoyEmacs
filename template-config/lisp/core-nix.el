;;; core-nix.el --- Nix language plumbing (TS, LSP, format) -*- lexical-binding: t; -*-
;;; Commentary:
;; - Prefer Tree-sitter major mode (nix-ts-mode) when available
;; - LSP via Eglot using the 'nil' language server
;; - Format-on-save: eglot-format-buffer -> alejandra -> nixfmt -> nixpkgs-fmt

;;; Code:

(require 'seq) ;; for seq-find

;; ---------- Modes & file associations ----------
;; Prefer nix-ts-mode; fall back to nix-mode; else fundamental-mode (so it never errors).
(add-to-list 'auto-mode-alist
             (cons "\\.nix\\'"
                   (cond
                    ((fboundp 'nix-ts-mode) 'nix-ts-mode)
                    ((fboundp 'nix-mode)    'nix-mode)
                    (t                      'fundamental-mode))))

;; Only add the remap if both modes exist (avoids “unknown mode 'nix-mode'” warning).
(when (and (boundp 'major-mode-remap-alist)
           (fboundp 'nix-mode)
           (fboundp 'nix-ts-mode))
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
  "Return the first available external Nix formatter executable, or nil."
  (seq-find #'executable-find core/nix-formatters))

(defun core/nix-format-buffer ()
  "Format current buffer. Prefer Eglot; otherwise use an external formatter if available."
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
  "Enable format-on-save for the current buffer."
  (add-hook 'before-save-hook #'core/nix-format-buffer nil t))

;; ---------- Eglot (LSP) ----------
(with-eval-after-load 'eglot
  ;; Tell Eglot to use 'nil' for Nix buffers.
  (add-to-list 'eglot-server-programs '((nix-mode nix-ts-mode) . ("nil"))))

(defun core/nix--eglot-setup ()
  "Ensure Eglot is running if available; enable Eldoc & Flymake."
  (when (featurep 'eglot)
    (unless (executable-find "nil")
      (message "core-nix: LSP server 'nil' not found on PATH"))
    (eglot-ensure))
  (when (fboundp 'eldoc-mode) (eldoc-mode 1))
  (when (fboundp 'flymake-mode) (flymake-mode 1)))

;; ---------- Per-buffer setup ----------
(defun core/nix-setup ()
  "Main setup for Nix buffers."
  (core/nix--eglot-setup)
  (core/nix--format-on-save)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2))

(add-hook 'nix-mode-hook     #'core/nix-setup)
(add-hook 'nix-ts-mode-hook  #'core/nix-setup)

;; ---------- Keybinding ----------
;; Handy manual format binding (global so it works in either mode).
(global-set-key (kbd "C-c =") #'core/nix-format-buffer)

(provide 'core-nix)
;;; core-nix.el ends here
