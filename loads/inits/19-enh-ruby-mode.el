;;; enh-ruby-mode

(autoload 'enh-ruby-mode "enh-ruby-mode"
  "Mode for editing ruby source files" t)
;(add-to-list 'auto-mode-alist '("\\.rb$latex " . enh-ruby-mode))
;(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
;(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

;; do endなどの対応関係を補完
(use-package ruby-electric
  :ensure t
  :config
  (add-hook 'enh-ruby-mode-hook '(lambda () (enh-ruby-electric-mode t)))
  (setq ruby-electric-expand-delimiters-list nil)
  )

;; ruby-block.el --- highlight matching block（endに対応する行）
(use-package ruby-block
  :ensure t
  :config
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t)
  )
