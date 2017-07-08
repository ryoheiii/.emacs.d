;;; enh-ruby-mode

(autoload 'enh-ruby-mode "enh-ruby-mode"
  "Mode for editing ruby source files" t)
;(add-to-list 'auto-mode-alist '("\\.rb$latex " . enh-ruby-mode))
;(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
;(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

;; do endなどの対応関係を補完
(require 'ruby-electric)
(add-hook 'enh-ruby-mode-hook '(lambda () (enh-ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

;; ruby-block.el --- highlight matching block（endに対応する行）
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; シンボルのハイライト
(require 'auto-highlight-symbol-config)
(require 'highlight-symbol)
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))

(global-set-key (kbd "") 'highlight-symbol-at-point)
(global-set-key (kbd "M-") 'highlight-symbol-remove-all)
