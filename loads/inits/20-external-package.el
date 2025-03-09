;;; 20-external-package.el --- 外部パッケージの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs の外部パッケージ設定

;;; Code:

;;;; [Group] Library - ライブラリ関連 ;;;;;;
;;; dash - Emacs 用のモダンなリスト操作ライブラリ
(use-package dash
  :straight t)

;;; s - 文字列操作のための便利なユーティリティ
(use-package s
  :straight t)

;;; diminish - モードラインの表示を最適化
(use-package diminish
  :straight t)



;;;;;; [Group] IME - 日本語入力の設定 ;;;;;
;;; Tr-ime - windows 用設定
(use-package tr-ime
  :straight t
  :if IS-WINDOWS
  :config
  (setq default-input-method "W32-IME")
  (tr-ime-standard-install)
  (w32-ime-initialize)
  )

;;; Mozc - mozc 設定
(use-package mozc
  :straight t
  :if (display-graphic-p)
  :unless IS-WINDOWS
  :init
  (setq default-input-method "japanese-mozc")
  )



;;;; [Group] Themes - テーマ関連 ;;;;;;
;;; color-theme-modern - モダンなカラーテーマの適用
;; (use-package color-theme-modern
;;   :straight t
;;   :config
;;   ;; 選択可能なテーマの幅を広げる
;;   ;; 参考: https://github.com/emacs-jp/replace-colorthemes/blob/master/screenshots.md
;;   (load-theme 'hober t t)
;;   (enable-theme 'hober)
;;   )

;;; doom-themes - テーマ設定
(use-package doom-themes
  :straight t
  :custom
  (doom-themes-enable-bold t)   ;; 強調された文字を有効化
  (doom-themes-enable-italic t) ;; イタリックを有効化
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  (font-lock-comment-face ((t (:foreground "#b0b0b0" :slant italic))))     ;; 薄めのグレー
  :config
  ;;; ロードテーマ
  ;; (load-theme 'doom-one t)
  (load-theme 'doom-dracula t)
  ;; (load-theme 'doom-gruvbox t)

  ;; 各種設定
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  ;; Treemacs に Doom のスタイルを適用（GUI環境のみ）
  (when (and (display-graphic-p) (fboundp 'treemacs))
    (doom-themes-treemacs-config))
  )

(global-set-key (kbd "<f6>") #'my/toggle-doom-theme)
(defun my/toggle-doom-theme ()
  "Doomテーマを light/dark で切り替える."
  (interactive)
  (if (eq (car custom-enabled-themes) 'doom-one)
      (progn (disable-theme 'doom-one) (load-theme 'doom-nord-light t))
    (disable-theme 'doom-nord-light)
    (load-theme 'doom-one t)))


;;; doom-modeline - モードラインのテーマ設定
(use-package doom-modeline
  :straight t
  :if (display-graphic-p)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  ;; (doom-modeline-major-mode-icon nil)
  ;; (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  ;; (line-number-mode 0)
  ;; (column-number-mode 0)
  )

;;; smart-mode-line - モードラインの外観と情報表示を最適化
(use-package smart-mode-line
  :straight t
  :init
  (sml/setup)
  :config
  (setq sml/no-confirm-load-theme t
        sml/theme 'dark
        sml/shorten-directory nil) ; ディレクトリパスはフル表示
  )

;;; hide-mode-line - 特定のモードでモードラインを非表示
(use-package hide-mode-line
  :straight t
  :hook ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode)
  )

;;; mode-line-bell - モードラインを利用した通知システム
(use-package mode-line-bell
  :straight t
  :hook (after-init . mode-line-bell-mode)
  )

;;; pulsar - カーソルの位置を明確にするためにエフェクトを追加
(use-package pulsar
  :straight t
  :if (display-graphic-p)
  :config
  (pulsar-global-mode +1)
  )

;;; goggles - 編集箇所をハイライト
(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)
  )

;;; spacious-padding - 画面に余白を付ける
(use-package spacious-padding
  :straight t
  :if (display-graphic-p)
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border))

  (spacious-padding-mode +1))

;;; perfect-margin - バッファが一つの時に中央寄せ
(use-package perfect-margin
  :straight t
  :config
  (setq perfect-margin-ignore-filters nil)
  (perfect-margin-mode +1)
  )

;;; volatile-highlights - 一時的なハイライト（選択範囲など）を強調表示
(use-package volatile-highlights
  :straight t
  :hook (after-init . volatile-highlights-mode)
  :custom-face
  (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD"))))
  )

;;; highlight-indent-guides - インデントレベルを視覚的に区別するためのガイド
(use-package highlight-indent-guides
  :straight t
  :if (version< emacs-version "29")  ;; Emacs 29 以降は無効（位置ズレが発生するため）
  :hook (emacs-lisp-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character))

;;; highlight-symbol - シンボルのハイライトとナビゲーション
(use-package highlight-symbol
  :straight t
  :bind (([f3] . highlight-symbol-at-point)
         ([f4] . highlight-symbol-remove-all))
  :config
  (setq highlight-symbol-colors '("DeepSkyBlue1" "LimeGreen" "HotPink1" "Yellow"
                                  "Cyan" "OrangeRed1" "MediumOrchid1" "SkyBlue"))
  )

;;; Rainbow-delimiters - 括弧の色分けで対応関係を視覚化
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :bind (("C-c l" . rainbow-delimiters-using-stronger-colors))  ;; より強調した色に変更
  :config
  ;; 強調した括弧の色を適用する関数
  (defun rainbow-delimiters-using-stronger-colors ()
    (interactive)
    (cl-loop for index from 1 to rainbow-delimiters-max-face-count
             do (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
                  (cl-callf color-saturate-name (face-foreground face) 50))))
  ;; 一致しない括弧をより目立たせる
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground     'unspecified
                      :inherit        'error
                      :strike-through t)
  )

;;; Nerd-icons - グラフィック環境向けのアイコン
(use-package nerd-icons
  :straight t
  :if (display-graphic-p)
  )

;;; Nerd-icons-completion - vertico 等の補完パッケージでアイコンを表示
(use-package nerd-icons-completion
  :straight t
  :if (display-graphic-p)
  :hook (after-init . nerd-icons-completion-mode)
  )

;;; Nerd-icons-dired - dired でアイコンを表示
(use-package nerd-icons-dired
  :straight t
  :if (display-graphic-p)
  :hook (dired-mode . nerd-icons-dired-mode)
  )

;;; Nyan-mode バッファ上での位置をニャンキャットが教えてくれる
(use-package nyan-mode
  :straight t
  :if (display-graphic-p)
  :init
  (setq nyan-bar-length 24)
  (nyan-mode +1)
  )

;;; Minions - マイナーモードをハンバーガーメニューで表示
(use-package minions
  :straight t
  :if (display-graphic-p)
  :init
  (minions-mode +1)
  )

;;; page-break-lines ^Lの改ページ文字の表示を良くする
(use-package page-break-lines
  :straight t
  :config
  (page-break-lines-mode +1)
  )

;;; highlight-defined - 既知のシンボルに色を付ける
(use-package highlight-defined
  :straight t
  :hook (emacs-lisp-mode . highlight-defined-mode)
  )

;;; highlight-quoted - 引用符と引用記号に色を付ける
(use-package highlight-quoted
  :straight t
  :hook (emacs-lisp-mode . highlight-quoted-mode)
  )



;;;;; [Group] Org - Org 関係 ;;;;;
;;; Org - org 設定
(use-package org
  :straight nil
  :hook ((org-mode . visual-line-mode))  ; 自動改行の有効化
  :defer t
  :custom
  (org-return-follows-link t)            ; Returnキーでリンク先を開く
  (org-hide-leading-stars t)             ; 見出しの*を非表示
  (org-startup-indented t)               ; インデント表示をデフォルトで有効化
  (org-src-fontify-natively t)           ; ソースコードをシンタックスハイライト
  (org-src-tab-acts-natively t)          ; org-babelでタブキーを言語モードに連動
  (org-edit-src-content-indentation 2)   ; org-babelのソースコードインデント
  (org-startup-folded 'content)          ; 初期表示で折りたたむ
  (org-log-done 'time)                   ; タスク完了時に時間を記録
  (org-log-into-drawer t)                ; ログを :LOGBOOK: に格納
  (org-adapt-indentation nil)            ; インデントの自動調整をオフにする
  (org-cycle-separator-lines 2)          ; 見出しの間隔
  (org-ellipsis " ▼")                   ; 折りたたみ表示の記号変更
  (org-agenda-files '("~/org/agenda/"))  ; アジェンダファイルのディレクトリ
  (org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  )

;;; org-indent - インデントを自動調整
(use-package org-indent
  :straight nil
  :hook (org-mode . org-indent-mode)
  )

;;; org-modern - 全体的なUI向上 (*) org-indent-mode と相性が悪いため一旦無効化
(use-package org-modern
  :disabled t
  :straight t
  :after (org)
  :hook (org-mode . org-modern-mode)
  :config
  (setopt
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setopt org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  (global-org-modern-mode)
  )

;;; org-download - 画像のクリップボード貼り付け
(use-package org-download
  :straight t
  :after (org)
  :bind (:map org-mode-map
              ("C-c i" . org-download-clipboard))
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "~/org/images/")
  (org-download-heading-lvl nil)
  (org-download-timestamp "_%Y%m%d-%H%M%S")
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  )

;;; org-roam - ノート管理
(use-package org-roam
  :straight t
  :after (org)
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-database-connector 'sqlite)
  (org-roam-db-location (expand-file-name "org-roam.db") my-db-dir) ;; my-db-dir @early-init.el
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  )

;;; org-agenda - スケジュール管理
(use-package org-agenda
  :straight nil
  :after (org)
  :bind ("C-c a" . org-agenda)
  :custom
  (org-agenda-files (directory-files-recursively "~/org/agenda/" "\\.org$"))
  (org-agenda-start-on-weekday nil)
  (org-agenda-span 'week)
  (org-agenda-use-time-grid t)
  (org-agenda-time-grid '((daily today)
                          (800 1000 1200 1400 1600 1800 2000)
                          "......" "----------------"))
  )





;;;;; [Group] Markdown - Markdown 関連 ;;;;;
;;; 変数定義（コンフィギュレーション）
;; Markdown 用のカスタム CSS ファイルのパス
(defvar my-markdown-css-file
  (my-set-custom "css/markdown-style.css")
  "Path to custom markdown CSS file.")

;; CSS を遅延ロードしつつ HTML に埋め込む関数
(defun my-markdown-load-css ()
  "Load the content of the markdown CSS file."
  (when (file-exists-p my-markdown-css-file)
    (with-temp-buffer
      (insert-file-contents my-markdown-css-file)
      (buffer-string))))

;; C-c TAB の制御を変更する関数
(defun my-markdown-insert-tab ()
  "C-c TAB を押したときに 4 スペースを挿入する"
  (interactive)
  (insert "    "))

;; Pandoc コマンドの設定
;; その他 option: "--toc --toc-depth=2", "--highlight-style=tango"
(defun my-get-pandoc-version ()
  "Pandoc のバージョン番号を取得し、数値で返す。"
  (let* ((version-str (shell-command-to-string "pandoc --version | head -n 1 | awk '{print $2}'"))
         (version-num (when (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)" version-str)
                        (string-to-number (match-string 1 version-str)))))
    version-num))

(setq my-markdown-pandoc-command
      (if (and (my-get-pandoc-version) (>= (my-get-pandoc-version) 3))
          (concat "pandoc -s --number-sections --toc --toc-depth=3 --embed-resources --standalone -f markdown -t html5"
                  " --css " my-markdown-css-file)
        (concat "pandoc -s --number-sections --toc --toc-depth=3 --self-contained -f markdown -t html5"
                " --css " my-markdown-css-file)))

;;; markdown-mode - markdown mode の設定
(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . markdown-mode)
  :hook ((markdown-mode . flyspell-mode)                              ; スペルチェック
         (markdown-mode . visual-line-mode)                           ; 行の折り返し
         (markdown-mode . (lambda ()                                  ; markdown-indent-on-enter
                            (setq markdown-indent-on-enter t)))
         (markdown-mode . display-line-numbers-mode)                  ; 行番号表示
         (markdown-mode . outline-minor-mode)                         ; 見出しの折りたたみ
         (markdown-mode . (lambda ()
                            (setq markdown-enable-math t)             ; 数式を有効化
                            markdown-fontify-code-blocks-natively t)) ; コードブロックの色付け
         (markdown-mode . electric-pair-mode)                         ; 括弧の自動補完
         (markdown-mode . (lambda ()
                            (local-set-key (kbd "C-c TAB") #'my-markdown-insert-tab))) ; 4 スペースを挿入
         )
  :custom
  (markdown-indent-level 4)
  (markdown-link-space-substitution-method 'underscores) ; リンクのスペースをアンダースコアに置換
  (markdown-header-scaling t)                            ; 見出しサイズの自動調整
  ;; markdown コマンドを pandoc に置き換え
  (markdown-command my-markdown-pandoc-command)          ; Pandoc で Markdown をエクスポート
  (markdown-export-command my-markdown-pandoc-command)
  (markdown-xhtml-header-content
   (format "<meta charset='utf-8'>\n
   <meta name='viewport' content='width=device-width, initial-scale=1'>\n
   <title>Markdown Export</title>\n
   <style>\n%s\n</style>\n
   <script src='https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.0/highlight.min.js'></script>\n
   <script>hljs.configure({languages: []});hljs.highlightAll();</script>\n"
   (my-markdown-load-css)))
  ;; コードブロックのシンタックスハイライト
  (markdown-code-lang-modes
   '(("bash"   . shell-script)
     ("elisp"  . emacs-lisp)
     ("python" . python)))
  ;; 画像を表示する設定
  (markdown-image-use-cache t) ; キャッシュして表示
  :bind (("C-c C-v h" . markdown-insert-header-dwim)     ; 見出しを挿入
         ("C-c C-v l" . markdown-insert-link)            ; リンクを挿入
         ("C-c C-v c" . markdown-insert-gfm-code-block)  ; コードブロックを挿入
         ("C-c C-v d" . markdown-insert-details))        ; 折り畳み項目を挿入
  :config
  ;; 折りたたみ HTML タグを挿入する関数
  (defun markdown-insert-details ()
    "Insert <details> HTML tag with a <summary>."
    (interactive)
    (insert "<details><summary>text</summary><div>\n\n</div></details>"))
  )

;;; markdown-toc - 目次生成
(use-package markdown-toc
  :straight t
  :after markdown-mode
  :hook ((markdown-mode . markdown-toc-mode)          ; テーブルの自動整形
         ;; (markdown-mode . markdown-toc-insert-toc) ; 保存時に目次を自動挿入
         ;; (before-save . markdown-toc-update-toc)   ; 保存前に目次を更新
         )
  :bind (("C-c C-v t" . markdown-toc-generate-toc))   ; 目次を手動で生成
  :custom
  (markdown-toc-without-markdown-toc t)               ; コメントを含めない
  (markdown-toc-headline-style 'atx)                  ; 見出しのスタイル
  )

;;; pandoc-mode - Markdown を他の形式に変換
(use-package pandoc-mode
  :straight t
  :commands (pandoc-mode)
  :hook (markdown-mode . pandoc-mode)
  )



;;;;; [Group] Completion UI - 補完 UI 関連
;;; Vertico - ミニバッファ補完 UI
(use-package vertico
  :straight t
  :init (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 30)
  (vertico-resize nil)
  (enable-recursive-minibuffers t)
  )

;;; Vertico-repeat - 直前の補完を再実施
(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :init
  (with-eval-after-load 'meow
    (meow-leader-define-key '("z" . vertico-repeat)))
  )

;;; Vertico-directory - ディレクトリ補完
(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("C-l" . vertico-directory-delete-char))
  )

;;; Vertico-truncate - vertico の補完候補を横方向に切り詰める
(use-package vertico-truncate
  :straight (:host github :repo "jdtsmith/vertico-truncate")
  :config
  (vertico-truncate-mode +1)
  (setq vertico-truncate-length 50)   ;; 最大表示幅 (デフォルト: 40)
  (setq vertico-truncate-suffix "…") ;; 省略記号 (デフォルト: "…")
  )


;;; Marginalia - 詳細な補完情報の表示
(use-package marginalia
  :straight t
  :after vertico
  :init (marginalia-mode)
  )

;;; Consult - 多機能ミニバッファ補完
(use-package consult
  :straight t
  :after vertico
  :bind
  (("C-x C-f" . find-file)
   ("C-x f"   . consult-find)
   ("C-x C-r" . consult-recent-file)
   ("C-x C-y" . consult-yank-pop)
   ("C-x b"   . consult-buffer)
   ("C-x i"   . consult-imenu)
   ("C-s"     . consult-line)
   ("C-S"     . consult-line-multi)
   ("C-."     . consult-goto-line)
   ;; ("C-x g"   . consult-grep)
   ("C-x g"   . affe-grep)
   )
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  )

;;; Consult-yasnippet - Yasnippet の `consult` インテグレーション
(use-package consult-yasnippet
  :straight t
  :bind ("C-c y" . consult-yasnippet)
  )

;;; Embark - ミニバッファアクション
(use-package embark
  :straight t
  :bind
  (("M-a" . embark-act))
  )

;;; Embark-consult - `embark` と `consult` の連携
(use-package embark-consult
  :straight t
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  )

;;; Helm-gtags - ソースコード内のシンボル検索とナビゲーション (Xref で置き換えたい)
(use-package helm-gtags
  :straight t
  :defer t
  :hook ((c-mode   . helm-gtags-mode)
         (c++-mode . helm-gtags-mode))
  :bind (:map helm-gtags-mode-map
              ("C-t d"   . helm-gtags-find-tag)         ; 関数の定義場所の検索 (define)
              ("C-t C-d" . helm-gtags-find-tag)
              ("C-t u"   . helm-gtags-find-rtag)        ; 関数の使用箇所の検索 (use)
              ("C-t C-u" . helm-gtags-find-rtag)
              ("C-t v"   . helm-gtags-find-symbol)      ; 変数の使用箇所の検索 (variable)
              ("C-t C-v" . helm-gtags-find-symbol)
              ("C-t f"   . helm-gtags-find-files)       ; ファイルの検索 (find)
              ("C-t C-f" . helm-gtags-find-files)
              ("C-t p"   . helm-gtags-previous-history) ; 前の履歴へ移動 (previous)
              ("C-t C-p" . helm-gtags-previous-history)
              ("C-t n"   . helm-gtags-next-history)     ; 次の履歴へ移動 (next)
              ("C-t C-n" . helm-gtags-next-history))
  :custom
  (helm-gtags-path-style 'root)  ; プロジェクトルート基準でタグを検索
  ;; (helm-gtags-ignore-case t)     ; 大文字小文字を区別しない検索
  ;; (helm-gtags-auto-update t)     ; ファイル変更時に自動でタグを更新
  :config
  ;; 'global -uv' を用いた GTAGS の自動更新
  (defun update-gtags ()
    "Update GTAGS database."
    (interactive)
    (when (and (buffer-file-name) (executable-find "global"))
      (start-process "gtags-update" nil "global" "-uv")))
  )

;;; Project - プロジェクト管理
(use-package project
  :bind (("C-x p f" . project-find-file))
  )

;;; Affe - 高速検索（grep 代替）
(use-package affe
  :straight t
  :bind (("C-x g" . affe-grep))
  :config
  (setq affe-regexp-function #'orderless-pattern-compiler)
  (setq affe-highlight-function #'orderless-highlight-matches)
  )

;;; Orderless - 高度な補完フィルタ
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  :config
  ;; migemo を利用したローマ字検索
  (with-eval-after-load 'migemo
    (defun orderless-migemo (component)
      (let ((pattern (downcase (migemo-get-pattern component))))
        (condition-case nil
            (progn (string-match-p pattern "") pattern)
          (invalid-regexp nil))))
    (add-to-list 'orderless-matching-styles 'orderless-migemo))

  ;; corfuはorderless-flexで絞り込む
  (with-eval-after-load 'corfu
    (defun orderless-fast-dispatch (word index total)
      (and (= index 0) (= total 1) (length< word 4)
           'orderless-literal-prefix))

    (orderless-define-completion-style orderless-fast
      (orderless-style-dispatchers '(orderless-fast-dispatch))
      (orderless-matching-styles '(orderless-flex)))

    (defun my/setup-corfu-for-orderless ()
      (setq-local corfu-auto-delay 0
                  corfu-auto-prefix 1
                  completion-styles '(orderless-fast)))

    (add-hook 'corfu-mode-hook #'my/setup-corfu-for-orderless))
  )

;;; Corfu - 補完 UI
(use-package corfu
  :straight t
  :init (global-corfu-mode)
  :bind
  (("C-M-i" . completion-at-point)
   :map corfu-map
   ("TAB"   . corfu-insert)
   ("<tab>" . corfu-insert)
   ("C-n"   . corfu-next)
   ("C-p"   . corfu-previous)
   ("C-s"   . corfu-scroll-down)
   ("M-n"   . nil)
   ("M-p"   . nil)
   )
  :custom
  (corfu-auto t)                              ; 自動補完
  (corfu-auto-delay 0)                        ; 入力後すぐに補完を表示
  (corfu-auto-prefix 2)                       ; プレフィックス長
  (corfu-cycle t)                             ; 候補リストのループ
  (corfu-on-exact-match nil)
  (corfu-preview-current nil)                 ; 現在の候補をプレビューしない
  (corfu-preselect 'prompt)                   ; 最初の候補を選択
  (corfu-scroll-margin 2)                     ; スクロールマージン
  (corfu-separator ?\s)                       ; orderless 用のセパレータ
  (completion-ignore-case t)                  ; 大文字小文字を区別しない
  (tab-always-indent 'complete)
  :config
  ;; c-mode などの一部のモードではタブに `c-indent-line-or-region` が割り当てられているので、
  ;; 補完が出るように `indent-for-tab-command` に置き換える
  (defun my/corfu-remap-tab-command ()
    (global-set-key [remap c-indent-line-or-region] #'indent-for-tab-command))
  (add-hook 'c-mode-hook #'my/corfu-remap-tab-command)
  (add-hook 'c++-mode-hook #'my/corfu-remap-tab-command)
  (add-hook 'java-mode-hook #'my/corfu-remap-tab-command)

  ;; ミニバッファー上でverticoによる補完が行われない場合、corfu の補完が出るように
  ;; https://github.com/minad/corfu#completing-in-the-minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (with-eval-after-load 'meow
    (define-key corfu-map (kbd "<escape>")
                (lambda ()
                  (interactive)
                  (corfu-quit)
                  (meow-normal-mode))))

  ;; lsp-modeでcorfuが起動するように設定する
  (with-eval-after-load 'lsp-mode
    (setq lsp-completion-provider :none))

  (with-eval-after-load 'orderless
    (defun my/orderless-for-corfu ()
      (setq-local orderless-matching-styles '(orderless-flex)))

    (add-hook 'corfu-mode-hook #'my/orderless-for-corfu))
  )

;;; Corfu-terminal - 端末 (`-nw`) で `corfu` を有効化
(use-package corfu-terminal
  :straight (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :after corfu
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode +1)
  )

;;; corfu-popupinfo - 補完候補の横に説明用のポップアップを表示
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :if (display-graphic-p)
  :hook (corfu-mode . corfu-popupinfo-mode)
  )

;;; Cape - `corfu` の補完バックエンド
(use-package cape
  :straight t
  :hook (;; ((prog-mode text-mode conf-mode lsp-completion-mode) . my/set-super-capf)
         (emacs-lisp-mode . my/set-elisp-capf)
         (after-init . my/global-capf))
  :config
  ;; 全モード共通の補完関数
  (defun my/global-capf ()
    "Set up global `completion-at-point-functions` for all modes."
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
    (advice-add 'lsp-completion-at-point :around #'cape-wrap-buster)
    (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
    (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-keyword)
    (add-hook 'completion-at-point-functions #'cape-file)
    )

  ;; ;; プログラミング系モードの補完
  ;; (defun my/set-super-capf ()
  ;;   "Set up `completion-at-point-functions` with `cape`."
  ;;   (setq-local completion-at-point-functions
  ;;               (list (cape-capf-super
  ;;                      )))) ;; 現状何もなし

  ;; Emacs Lisp の補完
  (defun my/set-elisp-capf ()
    "Set up `completion-at-point-functions` for `elisp-mode`."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'cape-elisp-symbol
                       #'cape-elisp-block))))

  ;; `M-x eval-expression` (`C-x C-e` など) でも補完可能に
  (add-hook 'eval-expression-minibuffer-setup-hook #'cape-elisp-symbol)

  ;; `yas-minor-mode` 有効時のみ cape-yasnippet を補完関数に追加
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-yasnippet t)))

  ;; dabbrevのサイズを制限
  (setq dabbrev-friend-buffer-function (lambda (other-buffer)
                                         (< (buffer-size other-buffer) (* 1024 1024))))
  )

;;; Flx - 柔軟なスコアリング
(use-package flx
  :straight t
  :config
  (with-eval-after-load 'prescient
    ;; 入力文字を抽出
    (defvar-local my/input-query nil)
    (defun my/store-input-query (string &rest _args)
      "Store the current completion query in `my/input-query'."
      (setq my/input-query (replace-regexp-in-string " " "" string)))
    (advice-add 'completion-all-completions :before #'my/store-input-query)

    ;; ローカル変数を使用できるように再定義
    (defvar vertico--total nil)
    (defvar corfu--total nil)

    ;; スコアのキャッシュ
    (defvar my/flx-cache (make-hash-table :test 'equal :size 1000))

    (defun my/get-flx-score (str query)
      (or (gethash (cons str query) my/flx-cache)
          (let ((score (condition-case nil
                           (car (flx-score str query flx-file-cache))
                         (error nil))))
            (puthash (cons str query) score my/flx-cache)
            score)))

    (defun my/flx-tiebreaker (c1 c2)
      (let ((total (or vertico--total corfu--total 0))
            (query-length (length my/input-query)))
        (if (and (< total 3000)
                 (> query-length 2)
                 (< (length c1) 100)
                 (< (length c2) 100))
            (let ((score1 (my/get-flx-score c1 my/input-query))
                  (score2 (my/get-flx-score c2 my/input-query)))
              (cond ((and (integerp score1) (integerp score2))
                     (cond ((> score1 score2) -1)
                           ((< score1 score2) 1)
                           (t (- (length c1) (length c2)))))
                    (t 0)))
          (- (length c1) (length c2)))))

    (setq prescient-tiebreaker #'my/flx-tiebreaker)

    (defun my/clear-flx-cache ()
      (clrhash my/flx-cache))

    ;; キャッシュを1時間ごとにクリア
    (defvar my/flx-cache-timer nil)
    (setq my/flx-cache-timer
          (run-with-timer 3600 3600 #'my/clear-flx-cache)))
  )

;;; Prescient - 補完履歴とスコアリング
(use-package prescient
  :straight t
  :custom
  (prescient-aggressive-file-save t)
  (prescient-save-file (my-set-history "prescient-save.el"))
  :config
  (prescient-persist-mode +1)
  )

;;; Vertico-prescient - `vertico` 用のスコアリング
(use-package vertico-prescient
  :straight t
  :after vertico
  :config
  (setq vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode +1)
  )

;;; Corfu-prescient - `corfu` 用のスコアリング
(use-package corfu-prescient
  :straight t
  :after corfu
  :config
  (with-eval-after-load 'orderless
    (setq corfu-prescient-enable-filtering nil))
  (corfu-prescient-mode +1)
  )



;;;;; [Group] Buffer-and-File-management - バッファとファイル管理関連 ;;;;;
;;; xclip - クリップボードとの共有
(use-package xclip
  :if (and (not (display-graphic-p)) ; GUIではない
           (getenv "DISPLAY")        ; X11 の DISPLAY 変数がある
           (executable-find "xclip")) ; `xclip` がシステムにインストールされている
  :straight t
  :config
  (xclip-mode 1)
  )

;;; dashboard - Emacs のスタートアップ画面をカスタマイズ
(use-package dashboard
  :straight t
  :hook (after-init . dashboard-setup-startup-hook)
  )

;;; total-lines - バッファ内の総行数をモードラインに表示
(use-package total-lines
  :straight t
  :hook (after-init . global-total-lines-mode)
  :config
  (setq mode-line-front-space
        (append mode-line-front-space
                '((:eval (when (bound-and-true-p total-lines)
                           (format " (%d)" (- total-lines 1))))))) ;; モードラインに総行数を表示
  )

;;; recentf - 最近使用したファイルの履歴管理
(use-package recentf
  :straight t
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 2000)                                 ; 保存するファイルの数
  (recentf-max-menu-items 15)                                    ; メニューに表示するアイテム数
  (recentf-exclude '("recentf-" user-full-name))                 ; 除外するファイルパターン
  (recentf-auto-cleanup 'never)                                  ; 自動整理の設定
  (recentf-save-file (my-set-history "recentf-" user-full-name)) ; recentf の保存パス
  :config
  ;; メッセージを抑制するマクロ
  (defmacro with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    `(let ((message-log-max nil))
       (with-temp-message (or (current-message) "") ,@body)))

  ;; 30秒ごとに recentf リストを自動保存
  (run-with-idle-timer 30 t 'recentf-save-list)
  )

;;; recentf-ext - recentf の拡張機能
(use-package recentf-ext
  :straight t
  )

;;; smooth-scroll - スムーズなスクロール
(use-package smooth-scroll
  :straight t
  :hook (after-init . smooth-scroll-mode)
  :custom
  (smooth-scroll/vscroll-step-size 8) ; スクロールのステップサイズ
  )

;;; anzu - 検索・置換時にマッチ数や現在位置を表示
(use-package anzu
  :straight t
  :hook (after-init . global-anzu-mode)
  :custom
  (anzu-mode-lighter "")                    ; モードラインの表示を非表示
  (anzu-deactivate-region t)                ; 領域選択時も `anzu` を使う
  (anzu-search-threshold 1000)              ; 最大検索数
  (anzu-replace-to-string-separator " => ") ; 置換時の表示フォーマット
  )



;;;;; [Group] Code-editting-and-Completion - コード編集と補完関連 ;;;;;
;;; Google C Style - Google の C スタイルガイドを適用
(use-package google-c-style
  :straight t
  :hook ((c-mode-common . google-set-c-style)
         (c-mode-common . google-make-newline-indent))
  )

;;; Aggressive Indent - コード編集時の自動インデント調整
(use-package aggressive-indent
  :straight t
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  )

;;; Smart Newline - 改行時の自動インデントと位置調整
;; 参考: https://ainame.hateblo.jp/entry/2013/12/08/162032
(use-package smart-newline
  :straight t
  :hook ((c++-mode c-mode cc-mode emacs-lisp-mode lisp-mode) . smart-newline-mode)
  :bind (("C-m" . smart-newline))
  )

;;; move-text - テキスト行の移動機能
(use-package move-text
  :straight t
  :bind (("C-M-p" . move-text-up)
         ("C-M-n" . move-text-down))
  )

;;; Irony - C/C++ のコード補完とシンボル情報の提供
(use-package irony
  :straight t
  :defer t
  :after cc-mode
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :custom
  ;; Irony モードのインストール場所とオプションファイルの設定
  (irony-server-install-prefix    (my-set-history "irony"))
  (irony-server-options-directory (my-set-history "irony"))
  )

;;; Yasnippet - コードスニペットの管理と挿入
(use-package yasnippet
  :straight t
  :custom
  (yas-prompt-functions '(yas-ido-prompt yas-no-prompt))  ; スニペット展開のプロンプト
  (yas-trigger-key "TAB")                                 ; トリガーキーを TAB に設定
  :config
  ;; ロードスニペットの設定
  (setq yas-snippet-dirs
        (seq-filter #'file-exists-p
                    (list (my-set-custom "snippets")
                          ;; シンボリックリンク用
                          (my-set-custom "snippets/snippets")
                          ;; yasnippet-snippets パッケージから取得
                          ;; (my-set-elisp "straight/build/yasnippet-snippets/snippets") ; 必要最小限に絞る
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/c++-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/c++-ts-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/c-lang-common")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/c-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/c-ts-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/cc-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/cmake-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/css-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/css-ts-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/dockerfile-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/emacs-lisp-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/git-commit-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/html-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/html-ts-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/lisp-interaction-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/lisp-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/makefile-automake-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/makefile-bsdmake-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/makefile-gmake-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/makefile-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/markdown-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/markdown-ts-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/org-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/prog-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/python-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/python-ts-mode")
                          (my-set-elisp "straight/build/yasnippet-snippets/snippets/text-mode")
                          )))
  (yas-global-mode 1)

  ;; Yasnippet Snippets - 追加スニペット集
  (use-package yasnippet-snippets :straight t)
  )

;;; Copilot - Github copilot による補完
(use-package copilot
  :disabled t
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-node-executable "~/.nvm/versions/node/v21.6.1/bin/node") ; Node.js の実行パス
  ;; *** Copilot の警告を抑止 ***
  ;; [Warning (copilot): ... copilot--infer-indentation-offset found no mode-specific indentation offset]
  ;; emacs-lisp-mode にて発生。copilot--indentation-alist への emacs-lisp-mode のダミー設定では抑制できず。
  ;; lisp-indent-offset への値設定にて抑制できるが、emacs-lisp-mode の思想に反し、
  ;; かつ aggresive-indent との競合が発生するため、警告抑止で対応する
  (warning-suppress-log-types '((copilot copilot-no-mode-indent)))  ; Copilot の警告抑制
  (warning-suppress-types '((copilot copilot-no-mode-indent)))
  :bind (:map copilot-mode-map
              ("C-M-<return>" . copilot-complete)            ; C-M-Enter で起動
              ("C-c c"        . copilot-clear-overlay)       ; C-c c     でオーバーレイをクリア
              ("C-c C-c"      . copilot-clear-overlay)       ; C-c C-c   でオーバーレイをクリア
              ("C-c i"        . copilot-panel-complete)      ; C-c i     で補完候補のパネル表示
              ("C-c C-i"      . copilot-panel-complete)      ; C-c C-i   で補完候補のパネル表示
              ("C-c p"        . copilot-previous-completion) ; C-c p     で前の補完候補を表示
              ("C-c C-p"      . copilot-previous-completion) ; C-c C-p   で前の補完候補を表示
              ("C-c n"        . copilot-next-completion)     ; C-c n     で次の補完候補を表示
              ("C-c C-n"      . copilot-next-completion)     ; C-c C-n   で次の補完候補を表示
              ("C-<return>"   . copilot-accept-completion))  ; C-Enter   で補完を受け入れる
  )

;;; Multiple Cursors - 複数カーソルによる編集機能
(use-package multiple-cursors
  :straight t
  :custom
  (mc/list-file (my-set-history "mc-lists.el"))
  :config
  ;; `repeat-mode` 用の `repeat-map` を作成
  (defvar-keymap my/mc-repeat-map
    :doc "Keymap for repeating multiple-cursors commands"
    :repeat t
    "p"  'mc/mark-previous-like-this
    "C-p" 'mc/mark-previous-like-this
    "n"  'mc/mark-next-like-this
    "C-n" 'mc/mark-next-like-this
    "*"  'mc/mark-all-like-this
    "a"  'mc/mark-all-like-this
    "C-a" 'mc/mark-all-like-this
    "d"  'mc/mark-all-like-this-dwim
    "C-d" 'mc/mark-all-like-this-dwim
    "m"  'mc/mark-more-like-this-extended
    "C-m" 'mc/mark-more-like-this-extended
    "u"  'mc/unmark-next-like-this
    "C-u" 'mc/unmark-next-like-this
    "U"  'mc/unmark-previous-like-this
    "s"  'mc/skip-to-next-like-this
    "C-s" 'mc/skip-to-next-like-this
    "S"  'mc/skip-to-previous-like-this
    "i"  'mc/insert-numbers
    "C-i" 'mc/insert-numbers
    "l"  'mc/insert-letters
    "C-l" 'mc/insert-letters
    "o"  'mc/sort-regions
    "C-o" 'mc/sort-regions
    "O"  'mc/reverse-regions)
  (global-set-key (kbd "C-q") my/mc-repeat-map) ; `C-q` をプレフィックスキーとして設定
  (repeat-mode 1) ; `repeat-mode` を有効化
  )

;;; Expand Region - 選択範囲をインクリメンタルに拡大・縮小
(use-package expand-region
  :straight t
  :custom
  (transient-mark-mode t) ; 明示的に選択範囲を表示
  :bind (("C-," . er/expand-region))
  )

;;; symbol-overlay - シンボルの置換
;; ※ Auto Highlight Symbol の ahs-edit-mode が Emacs 29 で正常に動作しないため置き換え
(use-package symbol-overlay
  :straight t
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("C-x C-a" . my-symbol-overlay-rename-visible)     ; ウィンドウ内のシンボルを置換
         ("C-x a"   . my-symbol-overlay-rename-in-function) ; 関数・メソッド内の置換
         ("C-x C-g" . symbol-overlay-rename))               ; バッファ全体のシンボルを置換
  :config
  ;; 現在のウィンドウ内のシンボルをリネーム
  (defun my-symbol-overlay-rename-visible ()
    "現在のウィンドウ内に表示されているシンボルのみをリネームする."
    (interactive)
    (let* ((symbol (symbol-overlay-get-symbol))
           (new-name (read-string (format "Rename '%s' to: " symbol)))
           (start (window-start))
           (end (window-end)))
      (save-excursion
        (goto-char start)
        (while (re-search-forward (regexp-quote symbol) end t)
          (replace-match new-name)))))

  ;; 現在の関数・メソッド内のシンボルをリネーム
  (defun my-symbol-overlay-rename-in-function ()
    "現在の関数やメソッド内に表示されているシンボルのみをリネームする."
    (interactive)
    (let* ((symbol (symbol-overlay-get-symbol))
           (new-name (read-string (format "Rename '%s' to: " symbol)))
           (start (cond
                   ((derived-mode-p 'c-mode 'c++-mode 'objc-mode)
                    (save-excursion (c-beginning-of-defun) (point)))
                   ((derived-mode-p 'python-mode)
                    (save-excursion (python-nav-beginning-of-defun) (point)))
                   (t (point-min)))) ;; その他のモードではファイル全体
           (end (cond
                 ((derived-mode-p 'c-mode 'c++-mode 'objc-mode)
                  (save-excursion (c-end-of-defun) (point)))
                 ((derived-mode-p 'python-mode)
                  (save-excursion (python-nav-end-of-defun) (point)))
                 (t (point-max))))) ;; その他のモードではファイル全体
      (save-excursion
        (goto-char start)
        (while (re-search-forward (regexp-quote symbol) end t)
          (replace-match new-name)))))
  )

;;;;; [Group] Navigation-and-Search - ナビゲーションと検索関連 ;;;;;
;;; Popwin - ポップアップウィンドウの管理
(use-package popwin
  :straight t
  :custom
  (popwin:popup-window-position 'bottom) ;; ポップアップの位置を下部に設定
  :init
  (popwin-mode 1)
  )

;;; Migemo - 日本語を含む検索時の挙動改善
(use-package migemo
  :straight t
  :if (executable-find "cmigemo")
  :custom
  (migemo-command "cmigemo")
  (igemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
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

;;;;; [Group] Languages-and-Style - 言語とスタイル関連 ;;;;;
;;; Mozc - 日本語入力の設定
(use-package mozc
  :straight nil
  :if window-system
  :config
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  )

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



;;;;; [Group] Version-control - バージョン管理関連 ;;;;;
;;; Magit
(use-package magit
  :straight t
  :ensure t
  :bind (("C-x G" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (defun mu-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  (bind-key "q" #'mu-magit-kill-buffers magit-status-mode-map)
  )

;;; Diff-hl - 変更点を表示 (git-gutter の置き換え)
(use-package diff-hl
  :straight t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :init
  (global-diff-hl-mode +1)
  (global-diff-hl-show-hunk-mouse-mode +1)
  (diff-hl-margin-mode +1)
  )

;;; Difftastic
(use-package difftastic
  :straight t
  :demand t
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)]))
  )

;;; with-editor - Emacs から Git コミットメッセージを編集
(use-package with-editor
  :straight t
  :defer t)

;;; Dsvn - SVN 管理ツール
(use-package dsvn
  :straight t
  )



;;;;; [Group] Misc-utilities - その他のユーティリティ ;;;;;
;;; Paradox - パッケージ管理 UI の強化
(use-package paradox
  :straight t
  :config
  (paradox-enable)
  :custom
  (paradox-github-token t)
  )

;;; Which-key - 使用可能なキーバインドの表示
(use-package which-key
  :straight t
  :custom
  (which-key-max-description-length 40) ; 説明の最大長
  (which-key-use-C-h-commands t)        ; C-h コマンドを使用
  :hook
  (after-init . which-key-mode)         ; Emacs 起動後に which-key モードを有効化
  )

;;; Free-keys - 未使用のキーバインドを表示
(use-package free-keys
  :straight t
  )

;;; Amx - M-x コマンドの履歴強化
(use-package amx
  :straight t
  :custom
  (amx-save-file (my-set-history "amx-items"))
  )

;;; undo-fu - undo と redo を強化
(use-package undo-fu
  :straight t
  :config
  (with-eval-after-load 'evil
    (setq evil-undo-system 'undo-fu))
  )

;;; undo-fu-session - undo 情報を Emacs 終了後も保持
(use-package undo-fu-session
  :straight t
  :custom
  (undo-fu-session-directory (my-set-history "undo-fu-session/"))
  :config
  (unless (file-exists-p undo-fu-session-directory)
    (make-directory undo-fu-session-directory t))
  (undo-fu-session-global-mode t)
  )

;;; Vundo - アンドゥの操作をツリー表示で管理
(use-package vundo
  :straight t
  :bind ("C-x u" . vundo)
  :custom
  (vundo-compact-display nil)  ; 画面を広めに使う
  (vundo-window-max-height 8)  ; vundo ウィンドウの高さを大きくする
  (vundo-roll-back-on-quit t)  ; `q` で抜けた時に元の位置に戻る
  :config
  (define-key vundo-mode-map (kbd "C-f") 'vundo-forward)  ; 次の状態へ (→)
  (define-key vundo-mode-map (kbd "C-b") 'vundo-backward) ; 前の状態へ (←)
  (define-key vundo-mode-map (kbd "C-n") 'vundo-next)     ; 下の分岐へ (↓)
  (define-key vundo-mode-map (kbd "C-p") 'vundo-previous) ; 上の分岐へ (↑)
  )

;;; Undohist - アンドゥ履歴のファイル保存
(use-package undohist
  :straight t
  :custom
  (undohist-directory (my-set-history "undohist"))
  :config
  (undohist-initialize)
  )

;;; Stopwatch - シンプルなストップウォッチ
(use-package stopwatch
  :straight (stopwatch
             :type git
             :host github
             :repo "blue0513/stopwatch") ; https://github.com/blue0513/stopwatch
  )

;;; Initchart - 初期化処理の実行時間を記録
(use-package initchart
  :straight (initchart
             :type git
             :host github
             :repo "yuttie/initchart") ; https://github.com/yuttie/initchart
  :config
  (initchart-record-execution-time-of load file)
  (initchart-record-execution-time-of require feature)
  )

(provide '20-external-package)
;;; 20-external-package.el ends here
