;;; Paren - 括弧の対応関係を視覚化する設定。カーソル位置の括弧ペアを強調表示
(use-package paren
  :ensure nil ;; built-in-package なのでインストール不要
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-style 'mixed) ;; ウィンドウ内に収まらないときだけ括弧内も光らせる
  )

;;; Whitespace - 空白文字の視覚化。スペース、タブ、行末の空白などを明確に表示
(use-package whitespace
  :ensure nil ;; built-in-package なのでインストール不要
  :defer t
  :diminish (global-whitespace-mode whitespace-mode)
  :init (global-whitespace-mode t)
  :config
  (setq whitespace-style '(face       ; faceを使って視覚化する。
                           trailing   ; 行末
                           tabs       ; タブ
                           spaces     ; スペース
                           empty      ; 先頭/末尾の空行
                           space-mark ; 表示のマッピング
                           tab-mark
                           newline))
  (setq whitespace-display-mappings
        '(
          (space-mark ?\x3000 [?\u25a1])           ; 全角スペース
          (tab-mark   ?\t [?\u00BB ?\t] [?\\ ?\t]) ; タブ
          ))
  (setq whitespace-display-mappings '((space-mark ?\x3000 [?\u25a1]) (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (setq whitespace-space-regexp "\\(\u3000+\\)") ;; スペースは全角のみを可視化
  (setq whitespace-trailing-regexp "\\([ \u00A0]+\\)$")
  (set-face-attribute 'whitespace-trailing nil :background "Black" :foreground "DeepPink"    :underline t)
  (set-face-attribute 'whitespace-tab nil      :background "Black" :foreground "DarkMagenta" :underline t)
  (set-face-attribute 'whitespace-space nil    :background "Black" :foreground "GreenYellow" :weight 'bold)
  (set-face-attribute 'whitespace-empty nil    :background "Black")
  )

;;; Grep - ファイル内検索機能の設定。特定のパターンに基づいてファイルを検索
(use-package grep
  :ensure nil ;; built-in-package なのでインストール不要
  :bind ("C-c g" . grep)
  :config
  (setq grep-command-before-query "grep -nr -e ")
  (defun grep-default-command ()
    (if current-prefix-arg
        (let ((grep-command-before-target
               (concat grep-command-before-query
                       (shell-quote-argument (grep-tag-default)))))
          (cons (if buffer-file-name
                    (concat grep-command-before-target
                            " *."
                            (file-name-extension buffer-file-name))
                  (concat grep-command-before-target " ."))
                (+ (length grep-command-before-target) 1)))
      (car grep-command)))
  (setq grep-command (cons (concat grep-command-before-query " .")
                           (+ (length grep-command-before-query) 1)))
  )

;; Elec-pair - 括弧の自動補完設定。入力中の括弧を自動的にペアで補完
(use-package elec-pair
  :ensure nil ;; built-in-package なのでインストール不要
  :config
  (electric-pair-mode +1)
  )

;;; Uniquify - バッファ名のユニーク化。同名ファイルを開いた際にディレクトリ名で区別
;; (use-package uniquify
;;   :ensure nil ;; built-in-package なのでインストール不要
;;   :config
;;   (setq uniquify-buffer-name-style 'forward)
;;   (setq uniquify-separator "/")         ;; rename after killing uniquified
;;   (setq uniquify-after-kill-buffer-p t) ;; don't muck with special buffers
;;   (setq uniquify-ignore-buffers-re "^\\*")
;;   )

;; straight.el 導入による "Could not find package uniquify" エラー対策。
;; ensure nil していてもEmacs の起動時に use-package がパッケージの存在を確認するためにエラーが発生する模様
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")         ;; rename after killing uniquified
(setq uniquify-after-kill-buffer-p t) ;; don't muck with special buffers
(setq uniquify-ignore-buffers-re "^\\*")

;;; Savehist - 履歴の保存設定。検索履歴やコマンド履歴をファイルに保存
(use-package savehist
  :ensure nil ;; built-in-package なのでインストール不要
  :defer 20
  :init (savehist-mode 1)
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-autosave-interval 60             ;; save every minute
        savehist-file (my-set-history "savehist") ;; my-set-history @00-auto-file-place.el
        )
  )

;;; Saveplace - カーソル位置の保存。ファイルを再度開いた時に前回のカーソル位置を保持
(use-package saveplace
  :ensure nil ;; built-in-package なのでインストール不要
  :init (setq save-place-limit nil)
  :config
  (save-place-mode 1)
  (setq save-place-file (my-set-history "places")) ;; my-set-history @00-auto-file-place.el
  )

;;; Windmove - ウィンドウ間の移動設定。Shift + 矢印キーでウィンドウ間を移動
(use-package windmove
  :ensure nil ;; ビルトインパッケージなのでインストールは不要
  :config
  (windmove-default-keybindings) ;; Shift + 矢印キーでウィンドウ間を移動
  )
