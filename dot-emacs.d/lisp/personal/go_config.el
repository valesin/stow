;; set 4 tabs
;;(add-mode-hook 'go-mode-hook (lambda ()
;;			       (setq tab-width 4)))

;; enable snippets
(require 'yasnippet)
(add-hook 'go-mode-hook #'yas-minor-mode)
(add-to-list 'yas-snippet-dirs "/home/vko/Documents/Uni/Alg/")

;; gofmt at save
(add-hook 'before-save-hook 'gofmt-before-save)

