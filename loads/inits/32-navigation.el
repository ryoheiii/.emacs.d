;;; 32-navigation.el --- ナビゲーションと検索の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; ナビゲーション、検索、スペルチェック関連パッケージの設定

;;; Code:

;;;;; [Group] Navigation-and-Search - ナビゲーションと検索関連 ;;;;;
;;; Popwin - ポップアップウィンドウの管理
(use-package popwin
  :straight t
  :defer t
  :custom
  (popwin:popup-window-position 'bottom) ;; ポップアップの位置を下部に設定
  :init
  ;; popwin-mode は autoload 済み → タイマーでパッケージロード + :config 実行
  (run-with-idle-timer 0.5 nil #'popwin-mode 1)
  )

;;; Migemo - 日本語を含む検索時の挙動改善
(use-package migemo
  :straight t
  :if (executable-find "cmigemo")
  :custom
  (migemo-command "cmigemo")
  (migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (migemo-options '("-q" "--emacs"))
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  :config
  ;; migemo-init は autoload なし → パッケージは即ロード、重い初期化のみ遅延
  (run-with-idle-timer 1 nil #'migemo-init)
  )

;;; Neotree - ファイルツリー表示とナビゲーション
(use-package neotree
  :straight t
  :bind ([f8] . neotree-toggle)
  :custom
  (neo-theme 'ascii)              ;; アイコンを ASCII にする
  (neo-smart-open t)              ;; カレントディレクトリを自動的に開く
  (neo-autorefresh t)             ;; 自動更新を有効化
  (neo-window-width 35)           ;; ウィンドウ幅を 35 に設定
  :config
  ;; `neotree-toggle` をカスタマイズ
  (defun neotree-toggle ()
    "Toggle NeoTree, opening at the project root or current file."
    (interactive)
    (let ((project-dir (ignore-errors (projectile-project-root)))
          (file-name (buffer-file-name)))
      (if (neo-global--window-exists-p)
          (neotree-hide)
        (progn
          (neotree-show)
          (when project-dir (neotree-dir project-dir))
          (when file-name   (neotree-find file-name))))))
  )

;;; Imenu List - バッファ内のシンボルリスト表示
(use-package imenu-list
  :straight t
  :bind ("C-c i" . imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t) ; 開いたら自動でフォーカスを移動
  ;; (imenu-list-auto-resize t)            ; サイズを自動調整
  )

;;;;; [Group] Spell-check - スペルチェック関連 ;;;;;
;;; ispell - スペルチェック機能の設定と辞書の指定（flyspell のバックエンド）
(use-package ispell
  :straight nil
  :custom
  (ispell-silently-savep t) ;; ユーザー辞書の保存時に確認しない
  (ispell-skip-region-alist '(("[^\000-\377]+"))) ;; 日本語無視
  (ispell-dictionary "en_US")
  :config
  (cond
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell"
          ispell-extra-args '("-d" "en_US"))) ;; hunspell に適したオプション
   ((executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--sug-mode=ultra"))))
  )

;;; flyspell - リアルタイムスペルチェック機能（フロントエンド）
(use-package flyspell
  :straight nil
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode)                    ; Emacs Lisp を除外
                          (setq-local ispell-skip-region-alist '(("[^\000-\377]+"))) ; 日本語無視
                          (flyspell-prog-mode))))
         ((text-mode html-mode markdown-mode) . (lambda () (flyspell-mode -1)))      ; text-mode では無効化
         (find-file . (lambda ()
                        (when (> (buffer-size) 3000)                                 ; 3000行以上なら無効
                          (flyspell-mode -1)))))
  :bind (:map flyspell-mode-map
              ("C-," . nil)
              ("C-." . nil)
              ("C-;" . nil)
              ("C-c $" . nil))
  :custom
  (flyspell-issue-message-flag nil) ; ミニバッファメッセージ抑制
  )

;;; flyspell-correct - スペルチェックの補助ツール
(use-package flyspell-correct
  :straight t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c C-/" . flyspell-correct-wrapper)) ; C-/ で補正メニューを開く
  )

;;; flyspell-correct-popup - pop-up メニューで修正候補を選べるようにする
(use-package flyspell-correct-popup
  :straight t
  :after flyspell-correct
  :custom
  (flyspell-correct-interface #'flyspell-correct-popup)
  )

(provide '32-navigation)
;;; 32-navigation.el ends here
