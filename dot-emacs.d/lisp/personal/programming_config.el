(require 'company)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
(defun lsp-go-install-save-hooks()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
