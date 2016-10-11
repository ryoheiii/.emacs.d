;;; web-modeを使用
(setq load-path (cons "~/.emacs.d/elisp/site-lisp/html-helper-mode/" load-path))
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)

(require 'html-helper-mode)

;;; 適用する拡張子
;; (add-to-list 'auto-mode-alist '("\\.phtml$"     . html-helper-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . html-helper-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp$"       . html-helper-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . html-helper-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb$"       . html-helper-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?$"     . html-helper-mode))
