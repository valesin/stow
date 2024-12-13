;; Company Mode Configuration
(use-package company
  :ensure t
  :defer t
  :init
  ;; Description: Delay (in seconds) before suggestions popup. Set to 0 for immediate suggestions.
  (setq company-idle-delay 0
        ;; Description: Minimum prefix length for triggering completion.
        company-minimum-prefix-length 1))

;; Display Line Numbers in Programming Modes
(use-package display-line-numbers
  :ensure nil ;; Built-in package, no need to ensure
  :hook
  ;; Enable line numbers in programming modes
  (prog-mode . display-line-numbers-mode))

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs "~/Documents/Uni/Alg/go-snippets/")
  (yas-reload-all)
  )
  
  

