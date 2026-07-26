;;; 32-navigation.el --- ナビゲーションと検索の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; ナビゲーション、検索、スペルチェック関連パッケージの設定

;;; Code:

;; project-root は autoload されないため、コンパイラ警告のみ declare で抑える
;; (実行時は project-current の autoload が project.el をロードする)
(declare-function project-root "project" (project &optional maybe-prompt))

;;;;; [Group] Navigation-and-Search - ナビゲーションと検索関連 ;;;;;
;;; Popwin - ポップアップウィンドウの管理
(use-package popwin
  :straight t
  :defer 0.5
  :custom
  (popwin:popup-window-position 'bottom) ;; ポップアップの位置を下部に設定
  :config
  (popwin-mode 1)
  )

;;; Migemo - 日本語を含む検索時の挙動改善
(use-package migemo
  :straight t
  :if (executable-find "cmigemo")
  :defer 1
  :custom
  (migemo-command "cmigemo")
  (migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (migemo-options '("-q" "--emacs"))
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  :config
  (migemo-init)
  )

;;; Neotree - ファイルツリー表示とナビゲーション
(use-package neotree
  :straight t
  :bind ([f8] . my/neotree-project-toggle)
  :custom
  (neo-theme 'ascii)              ;; アイコンを ASCII にする
  (neo-smart-open t)              ;; カレントディレクトリを自動的に開く
  (neo-autorefresh t)             ;; 自動更新を有効化
  (neo-window-width 35)           ;; ウィンドウ幅を 35 に設定
  :config
  ;; プロジェクトルートまたは現在ファイルの位置で開くトグル
  (defun my/neotree-project-toggle ()
    "Toggle NeoTree, opening at the project root or current file."
    (interactive)
    (let ((project-dir (when-let ((proj (project-current)))
                         (project-root proj)))
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

  ;; ispell 辞書未整備時に CAPF エラーを抑制する
  (defvar my/ispell-capf-warned nil
    "Non-nil なら ispell CAPF エラー警告は表示済み.")

  (defun my/safe-ispell-completion-at-point (orig-fn &rest args)
    "ispell エラー時に初回のみ警告を出し nil を返して CAPF として無害化する."
    (condition-case err
        (apply orig-fn args)
      (error
       (unless my/ispell-capf-warned
         (setq my/ispell-capf-warned t)
         (message "ispell: completion error (%s)" (error-message-string err)))
       nil)))
  (unless (advice-member-p #'my/safe-ispell-completion-at-point 'ispell-completion-at-point)
    (advice-add 'ispell-completion-at-point :around #'my/safe-ispell-completion-at-point))
  )

;;; flyspell - リアルタイムスペルチェック機能（フロントエンド）
(use-package flyspell
  :straight nil
  ;; :hook から参照する関数は :preface で定義する (:init だと多重定義警告が出る)
  :preface
  (defun my/flyspell-prog-setup ()
    "Emacs Lisp を除くプログラムモードで flyspell-prog-mode を有効化する。"
    (unless (derived-mode-p 'emacs-lisp-mode)
      (setq-local ispell-skip-region-alist '(("[^\000-\377]+"))) ; 日本語無視
      (flyspell-prog-mode)))
  (defun my/flyspell-disable ()
    "flyspell を無効化する。"
    (flyspell-mode -1))
  (defun my/flyspell-disable-in-large-buffer ()
    "大きなバッファ (3000 文字超) では flyspell を無効化する。"
    (when (> (buffer-size) 3000)
      (flyspell-mode -1)))
  :hook ((prog-mode . my/flyspell-prog-setup)
         ((text-mode html-mode markdown-mode) . my/flyspell-disable) ; text 系では無効化
         (find-file . my/flyspell-disable-in-large-buffer))
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

(provide '32-navigation)
;;; 32-navigation.el ends here
