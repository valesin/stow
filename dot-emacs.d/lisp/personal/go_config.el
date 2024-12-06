;; Go Mode Configuration
(use-package go-mode
  :ensure t
  :defer t
  :hook ((go-mode . (lambda () (setq tab-width 4)))
         (go-mode . lsp-deferred)  ;; Start LSP when entering Go mode
         (go-mode . yas-minor-mode)
         (go-mode . (lambda ()
                     (add-hook 'before-save-hook #'gofmt-before-save nil t)
                     (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                     (add-hook 'before-save-hook #'lsp-organize-imports nil t)))))
	 
(use-package lsp-mode
  :ensure t :ensure lsp-ui
  :defer t
  :config
  (with-eval-after-load 'lsp-mode
    (lsp-register-custom-settings
     '(("golangci-lint.command"
        ["golangci-lint" "run" "--enable-all" "--disable" "lll" "--out-format" "json" "--issues-exit-code=1"])))

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection
                                      '("golangci-lint-langserver"))
                      :activation-fn (lsp-activate-on "go")
                      :language-id "go"
                      :priority 0
                      :server-id 'golangci-lint
                      :add-on? t
                      :library-folders-fn #'lsp-go--library-default-directories
                      :initialization-options (lambda ()
                                             (gethash "golangci-lint"
                                                      (lsp-configuration-section "golangci-lint")))))))
