;;; 自動補完機能-auto-complete
(use-package auto-complete
  :ensure t
  :demand t
  :diminish ""
  :bind (:map ac-menu-map
              ("C-n" . ac-next)
              ("C-p" . ac-previous))

  :config
  (use-package auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/my-data/ac-dict") ;; ディレクトリ指定
  (add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
  (ac-set-trigger-key "TAB")

  (setq ac-use-menu-map t)
  (setq ac-disable-faces nil) ;;コメントや文字列リテラルでも補完を行う
  (setq ac-use-fuzzy t)       ;; 曖昧マッチ

  ;; yasnippetのbindingを指定するとエラーが出るので回避する方法。
  (setf (symbol-function 'yas-active-keys)
        (lambda ()
          (remove-duplicates (mapcan #'yas--table-all-keys (yas--get-snippet-tables)))))
  )
