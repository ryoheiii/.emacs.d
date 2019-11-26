;; https://emacs-jp.github.io/packages/helm/helm-gtags.html
(require 'helm-config)
(require 'helm-gtags)

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'perl-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)
(add-hook 'clojure-mode-hook 'helm-gtags-mode)
(add-hook 'erlang-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
             ;; 指定した関数が定義されている部分をさがす
             (local-set-key [f11] 'helm-gtags-find-tag)
;              (local-set-key kbd "M-t" 'helm-gtags-find-tag)
             ;; 指定した関数が参照されている部分を探す
             (local-set-key [f12] 'helm-gtags-find-rtag)
;             (local-set-key "M-r" 'helm-gtags-find-rtag)
             ;; 指定した変数、定義の定義元を探す
             (local-set-key [f9] 'helm-gtags-find-symbol)
;              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
             ;; gtagsでジャンプする一つ前の状態に戻る
             (local-set-key "\C-t" 'helm-gtags-pop-stack)

             (local-set-key [f6] 'helm-gtags-find-files)
             ))
