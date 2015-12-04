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
(global-set-key (kbd "M-") 'highlight-symbol-remove-all

;;
;;rcodetools
;; インストール必要
;;;;; http://futurismo.biz/archives/2213
;; (require 'rcodetools)
;; (setq rct-find-tag-if-available nil)
;; (defun make-ruby-scratch-buffer ()
;;   (with-current-buffer (get-buffer-create "*ruby scratch*")
;;     (ruby-mode)
;;     (current-buffer)))
;; (defun ruby-scratch ()
;;   (interactive)
;;   (pop-to-buffer (make-ruby-scratch-buffer)))
;; (defun ruby-mode-hook-rcodetools ()
;;   (define-key ruby-mode-map "\M-\C-i" 'rct-complete-symbol)
;;   (define-key ruby-mode-map "\C-c\C-t" 'ruby-toggle-buffer)
;;   (define-key ruby-mode-map "\C-c\C-d" 'xmp)
;;   (define-key ruby-mode-map "\C-c\C-f" 'rct-ri))
;; (add-hook 'ruby-mode-hook 'ruby-mode-hook-rcodetools)

;;; rdefs
;; rdefsはソースコードのclassやmodule、defといった宣言のラインを引っ張り出してくれるツール。 
;; インストール必要
;; (require 'anything)
;; (require 'anything-rdefs)
;; (add-hook 'enh-ruby-mode-hook
;;           (lambda ()
;;             (define-key enh-ruby-mode (kbd "C-@") 'anything-rdefs)))
