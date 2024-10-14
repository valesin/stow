;; set 4 tabs
;;(add-mode-hook 'go-mode-hook (lambda ()
;;			       (setq tab-width 4)))

;; enable snippets
(require 'yasnippet)
(add-hook 'go-mode-hook #'yas-minor-mode)
(add-to-list 'yas-snippet-dirs "/home/vko/Documents/Uni/Alg/")

;; gofmt at save
(add-hook 'before-save-hook 'gofmt-before-save)

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
                                                       (lsp-configuration-section "golangci-lint"))))))
