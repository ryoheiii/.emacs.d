;;; yasnippetの設定
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("C-x i i" . yas-insert-snippet)     ;; 既存スニペットを挿入する
              ("C-x i n" . yas-new-snippet)        ;; 新規スニペットを作成するバッファを用意する
              ("C-x i v" . yas-visit-snippet-file) ;; 既存スニペットを閲覧・編集する
              ("C-x i l" . yas-describe-tables)
              ("C-x i g" . yas-reload-all))
  :config
  (progn
    (setq yas-snippet-dirs
          '("~/.emacs.d/my-data/snippets")
          ))
  (yas-global-mode 1)

  ;; 単語展開キーバインド (ver8.0から明記しないと機能しない)
  ;; (setqだとtermなどで干渉問題ありだった)
  ;; もちろんTAB以外でもOK 例えば "C-;"とか
  (custom-set-variables '(yas-trigger-key "TAB"))
  )

(use-package helm-c-yasnippet
  :ensure t
  :bind
  (("C-c y" . helm-yas-complete))
  :config
  (setq helm-yas-space-match-any-greedy t)
  )
