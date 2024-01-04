;;; 括弧
(use-package paren
  :ensure nil ; built-in-package なのでインストール不要
  :hook
  (after-init . show-paren-mode)
  :config
  (setq show-paren-style 'mixed) ;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
  )

;;; 空白
(use-package whitespace
  :ensure nil ; built-in-package なのでインストール不要
  :defer t
  :diminish
  (global-whitespace-mode whitespace-mode)
  :init
  (global-whitespace-mode t)
  :config
  (setq whitespace-style '(face       ; faceを使って視覚化する。
                           trailing   ; 行末
                           tabs       ; タブ
                           spaces     ; スペース
                           empty      ; 先頭/末尾の空行
                           space-mark ; 表示のマッピング
                           tab-mark
                           newline
                           ))

  (setq whitespace-display-mappings
        '(
          (space-mark ?\x3000 [?\u25a1])           ; 全角スペース
          (tab-mark   ?\t [?\u00BB ?\t] [?\\ ?\t]) ; タブ
          ))

  ;; スペースは全角のみを可視化
  (setq whitespace-space-regexp    "\\(\u3000+\\)")
  (setq whitespace-trailing-regexp "\\([ \u00A0]+\\)$")

  (defvar my/bg-color "Black")
  (set-face-attribute 'whitespace-trailing nil
                      :background my/bg-color
                      :foreground "DeepPink"
                      :underline t
                      )
  (set-face-attribute 'whitespace-tab nil
                      :background my/bg-color
                      :foreground "DarkMagenta"
                      :underline t
                      )
  (set-face-attribute 'whitespace-space nil
                      :background my/bg-color
                      :foreground "GreenYellow"
                      :weight 'bold
                      )
  (set-face-attribute 'whitespace-empty nil
                      :background my/bg-color
                      )
  )

(use-package grep
  :ensure nil ; built-in-package なのでインストール不要
  :bind (
         ("C-c g" . grep)
         )
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

;; 括弧の補完
(use-package elec-pair
  :ensure nil ; built-in-package なのでインストール不要
  :config
  (electric-pair-mode +1)
  )

;; highlight the current line
;; (use-package hl-line
;;   :ensure t
;;   :config
;;   (global-hl-line-mode +1))

;;; バッファ名
;; ファイル名が重複していたらディレクトリ名を追加する。
;(use-package uniquify
;  :ensure nil ; built-in-package なのでインストール不要
;  :config
;  (setq uniquify-buffer-name-style 'forward)
;  (setq uniquify-separator "/")
;  ;; rename after killing uniquified
;  (setq uniquify-after-kill-buffer-p t)
;  ;; don't muck with special buffers
;  (setq uniquify-ignore-buffers-re "^\\*")
;  )
;; straight.el 導入による "Could not find package uniquify" エラー対策。
;; ensure nil していてもEmacs の起動時に use-package がパッケージの存在を確認するためにエラーが発生する模様
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
;; rename after killing uniquified
(setq uniquify-after-kill-buffer-p t)
;; don't muck with special buffers
(setq uniquify-ignore-buffers-re "^\\*")

(use-package savehist
  :ensure nil ; built-in-package なのでインストール不要
  :defer 20
  :init
  (savehist-mode 1)
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (my-set-history "savehist") ;; my-set-history @00-auto-file-place.el
        )
  )

;; カーソルの場所を保存する
(use-package saveplace
  :ensure nil ; built-in-package なのでインストール不要
  :init
  (setq save-place-limit nil)
  (save-place-mode 1)
  :config
  (setq save-place-file (my-set-history "places"))        ;; my-set-history @00-auto-file-place.el
  )

(use-package windmove
  :ensure nil ; ビルトインパッケージなのでインストールは不要
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings)
  )
