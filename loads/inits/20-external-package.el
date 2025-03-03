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



;;;; [Group] Themes - テーマ関連 ;;;;;;
;;; color-theme-modern - モダンなカラーテーマの適用
(use-package color-theme-modern
  :straight t
  :config
  ;; 選択可能なテーマの幅を広げる
  ;; 参考: https://github.com/emacs-jp/replace-colorthemes/blob/master/screenshots.md
  (load-theme 'hober t t)
  (enable-theme 'hober)
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

;;; beacon - カーソルの位置を明確にするために点滅エフェクトを追加
(use-package beacon
  :straight t
  :custom (beacon-color "yellow")
  :config (beacon-mode 1)
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

;;; rainbow-delimiters - 括弧の色分けで対応関係を視覚化
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

;;; all-the-icons - グラフィック環境向けのアイコン
(use-package all-the-icons
  :straight t
  :if (display-graphic-p)
  )



;;;;; [Group] Org - Org 関係 ;;;;;
;;; Org - org 設定
(use-package org
  :straight nil
  :hook ((org-mode . visual-line-mode))  ; 自動改行の有効化
  :defer t
  :custom
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



;;;;; [Group] Markdown - Markdown 関係 ;;;;;
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

;; Pandoc コマンドの設定
;; その他 option: "--toc --toc-depth=2", "--highlight-style=tango"
(defvar my-markdown-pandoc-command
  (concat "pandoc -s --number-sections --self-contained -f markdown -t html5"
          " --css " my-markdown-css-file)
  "Pandoc command for Markdown export.")

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
         (markdown-mode . electric-pair-mode))                        ; 括弧の自動補完
  :custom
  (markdown-indent-level 2)
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
  :hook (emacs-lisp-mode . aggressive-indent-mode))

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

;;; company - 自動補完の基本設定
(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :bind (("C-M-i" . company-complete)
         :map company-mode-map
         ("TAB" . indent-for-tab-command)
         :map company-active-map
         ("M-n" . nil)                        ; M-n で次の候補への移動をキャンセル
         ("M-p" . nil)                        ; M-p で前の候補への移動をキャンセル
         ("C-n" . company-select-next)        ; 次の補完候補を選択
         ("C-p" . company-select-previous)    ; 前の補完候補を選択
         ("C-s" . company-filter-candidates)  ; C-s で絞り込む
         ("TAB" . company-complete-selection)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :custom
  (company-selection-wrap-around t)                            ; 最後の候補で次の候補にループ
  (company-transformers '(company-sort-by-occurrence
                          company-sort-by-backend-importance)) ; 頻度順ソート
  (company-idle-delay 0)                                       ; デフォルトは 0.5
  (company-show-numbers t)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 2)                            ; デフォルトは 4
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-dabbrev-around t)
  (completion-ignore-case t)
  (company-dabbrev-downcase nil)
  (company-eclim-auto-save nil)
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-echo-metadata-frontend))
  :config
  ;; ツールチップの色設定
  ;; 基本設定
  (set-face-attribute 'company-tooltip nil
                      :foreground "white" :background "midnight blue")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "white" :background "midnight blue")
  ;; 選択項目の設定
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "dark slate blue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "white" :background "dark slate blue")
  ;; プレビューとスクロールバーの設定
  (set-face-attribute 'company-preview-common nil
                      :background "dark slate blue" :foreground "white" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "dark gray")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "dim gray")

  ;; company-backends の設定
  (setq company-backends
        '((company-capf :with company-same-mode-buffers company-yasnippet)
          (company-dabbrev-code :with company-same-mode-buffers company-yasnippet)
          (company-irony-c-headers company-irony)   ; Irony と Irony-C-Headers の組み合わせ
          company-keywords
          company-files
          company-dabbrev))
  (add-to-list 'company-backends 'company-files)    ; ファイル名補完
  (add-to-list 'company-backends 'company-keywords) ; 言語のキーワード補完
  )

;;; Company-statistics - よく使う補完候補を優先表示
(use-package company-statistics
  :straight t
  :after company
  :custom
  (company-statistics-file (my-set-history "company-statistics-cache.el")) ; 履歴保存場所
  :config
  (company-statistics-mode)
  )

;;; company-same-mode-buffers - 同じモード内でのキーワード補完を強化
(use-package company-same-mode-buffers
  :straight '(company-same-mode-buffers
              :type git
              :host github
              :repo "zk-phi/company-same-mode-buffers")
  :after company
  :config
  (company-same-mode-buffers-initialize)
  )

;;; Company-irony - C/C++ 用の補完
(use-package company-irony
  :straight t
  :after (company irony)
  )

;;; Company-irony-c-headers - C ヘッダファイル用の補完（Company バックエンド）
(use-package company-irony-c-headers
  :straight t
  :after (company irony)
  )

;;; company-dwim - 候補の絞り込みをスマート化（補完フロントエンド）
(use-package company-dwim
  :straight '(company-dwim
              :type git
              :host github
              :repo "zk-phi/company-dwim")
  :after company
  :bind (:map company-active-map
              ("TAB" . company-dwim))
  :config
  (add-to-list 'company-frontends 'company-dwim-frontend)
  )

;;; company-anywhere - どこでも補完を可能にする
(use-package company-anywhere
  :straight '(company-anywhere
              :type git
              :host github
              :repo "zk-phi/company-anywhere")
  :after company
  )

;;; Company-box - Company の補完候補をパネル表示 (GUI 限定)
(use-package company-box
  :straight t
  :if (display-graphic-p)
  :hook (company-mode . company-box-mode)
  )

;;; company-posframe - GUI での補完候補パネル表示
(use-package company-posframe
  :straight t
  :if (display-graphic-p)
  :after company
  :config
  (company-posframe-mode 1)
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

;;; Helm-c-yasnippet - Helm 経由のスニペット選択
(use-package helm-c-yasnippet
  :straight t
  :bind (("C-c y" . helm-yas-complete))
  :custom
  (helm-yas-space-match-any-greedy t) ; Helm のスペースマッチングを有効化
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

;;; Helm - 効率的なバッファやコマンドの検索
(use-package helm
  :straight t
  :after migemo
  :init
  (helm-mode 1)
  (helm-migemo-mode 1)
  :bind (("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x C-y" . helm-show-kill-ring)
         ("C-x g"   . helm-do-grep-ag)
         ;; ("C-x b"   . helm-buffers-list)
         ("C-x b"   . helm-mini)
         ("C-x i"   . helm-imenu)
         :map helm-map
         ("C-h" . delete-backward-char)
         :map helm-find-files-map
         ("C-h" . delete-backward-char)
         ("TAB" . helm-execute-persistent-action)
         :map helm-read-file-map
         ("TAB" . helm-execute-persistent-action))
  :custom
  (helm-display-function #'display-buffer)       ; 'display-buffer' を使用
  (helm-echo-input-in-header-line t)             ; 入力をヘッダーラインに表示
  (helm-input-idle-delay 0.2)                    ; 入力から画面更新までの遅延時間 (デフォルトは 0.01 秒)
  (helm-split-window-inside-p t)                 ; Helm のウィンドウを現在のウィンドウ内に分割して表示
  (helm-move-to-line-cycle-in-source t)          ; バッファ内で候補の最後に到達したら先頭に戻る
  (helm-ff-file-name-history-use-recentf t)      ; 'recentf' をファイル名の履歴として使用
  (helm-ff-search-library-in-sexp t)             ; SEXP 内でライブラリ検索
  (helm-ff-fuzzy-matching nil)                   ; ファジーマッチを無効化
  (helm-buffer-details-flag nil)                 ; バッファの詳細を非表示
  (helm-delete-minibuffer-contents-from-point t) ; ミニバッファの内容を削除
  :config
  ;; Emacsのコマンドと履歴の Helm ソース定義
  (defvar helm-source-emacs-commands
    (helm-build-sync-source "Emacs commands"
      :candidates (lambda ()
                    (let ((cmds))
                      (mapatoms (lambda (elt) (when (commandp elt) (push elt cmds))))
                      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "A Helm source for Emacs commands.")

  (defvar helm-source-emacs-commands-history
    (helm-build-sync-source "Emacs commands history"
      :candidates (lambda ()
                    (let ((cmds))
                      (dolist (elem extended-command-history)
                        (push (intern elem) cmds))
                      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "Emacs commands history.")

  ;; 検索パターンの変換
  (defun helm-buffers-list-pattern-transformer (pattern)
    "Helm のバッファ検索時のパターン変換."
    (if (equal pattern "")
        pattern
      (let* ((first-char (substring pattern 0 1))
             (pattern (cond ((equal first-char "*") (concat " " pattern))
                            ((equal first-char "=") (concat "*" (substring pattern 1)))
                            (t pattern))))
        (setq pattern (replace-regexp-in-string "\\." "\\\\." pattern))
        (setq pattern (replace-regexp-in-string "\\*" "\\\\*" pattern))
        pattern)))

  ;; 'kill-line' のエミュレーション
  (advice-add 'helm-delete-minibuffer-contents
              :before
              (lambda ()
                "Emulate `kill-line` in Helm minibuffer."
                (kill-new (buffer-substring (point) (field-end)))))

  ;; 'helm-ff-kill-or-find-buffer-fname' をファイルが存在する場合のみ実行
  (advice-add 'helm-ff-kill-or-find-buffer-fname
              :around
              (lambda (orig-fun &rest args)
                "Execute command only if CANDIDATE exists."
                (when (file-exists-p (car args))
                  (apply orig-fun args))))

  ;; 'helm-ff--transform-pattern-for-completion' のカスタマイズ
  (advice-add 'helm-ff--transform-pattern-for-completion
              :around
              (lambda (orig-fun pattern)
                "Transform the pattern to reflect intention."
                (let* ((input-pattern (file-name-nondirectory pattern))
                       (dirname (file-name-directory pattern)))
                  (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
                  (concat dirname (if (string-match "^\\^" input-pattern)
                                      ;; '^' is a pattern for basename
                                      ;; and not required because the directory name is prepended
                                      (substring input-pattern 1)
                                    (concat ".*" input-pattern))))))


  ;; helm-migemo fix
  ;; http://emacs.rubikitch.com/helm-migemo/
  (with-eval-after-load "helm-migemo"
    (defun helm-compile-source--candidates-in-buffer (source)
      (helm-aif (assoc 'candidates-in-buffer source)
          (append source
                  `((candidates
                     . ,(or (cdr it)
                            (lambda ()
                              (helm-candidates-in-buffer (helm-get-current-source)))))
                    (volatile) (match identity)))
        source))
    (defalias 'helm-mp-3-get-patterns 'helm-mm-3-get-patterns)
    (defalias 'helm-mp-3-search-base 'helm-mm-3-search-base))
  )

;;; Helm-gtags - ソースコード内のシンボル検索とナビゲーション
(use-package helm-gtags
  :straight t
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

;;; Helm-ag - ファイルの内容を高速検索
(use-package helm-ag
  :straight t
  )

;;; Helm-swoop - インクリメンタルサーチ機能の強化
(use-package helm-swoop
  :straight t
  :bind (("C-x s" . helm-swoop))
  :custom-face
  ;; 検索行のハイライト色を設定（例：明るい青の背景と白のテキスト）
  (helm-swoop-target-line-face ((t (:background "#0077ff" :foreground "white"))))
  ;; 検索語のハイライト色を設定（例：明るい緑の背景と黒のテキスト）
  (helm-swoop-target-word-face ((t (:background "#00ff00" :foreground "black"))))
  )

;;; Swiper - インクリメンタルサーチ機能の強化
(use-package swiper
  :straight t
  :bind (("C-s" . swiper))
  )

;;; Ivy - 効率的なバッファやファイルの検索 (swiper の強化)
(use-package ivy
  :straight t
  :after ivy-migemo
  :init
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-height 30)
  (ivy-extra-directories nil)
  ;; swiper と連携
  ;; https://github.com/ROCKTAKEY/ivy-migemo
  (ivy-re-builders-alist '((t . ivy--regex-plus)
                           (swiper . ivy-migemo--regex-plus)
                           ;; (t . ivy--regex-fuzzy)             ; fuzzy バージョン
                           ;; (swiper . ivy-migemo--regex-fuzzy) ; fuzzy バージョン
                           ))
  )

;;; Ivy-migemo - Ivy で Migemo を利用 (Swiper/Ivy の強化)
(use-package ivy-migemo
  :straight t
  )

;;; Migemo - 日本語を含む検索時の挙動改善
(use-package migemo
  :straight t
  :config
  (setq migemo-command "cmigemo"
        migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"
        migemo-options '("-q" "--emacs")
        migemo-user-dictionary nil
        migemo-regex-dictionary nil
        migemo-coding-system 'utf-8-unix)
  (migemo-init)
  )

;; まだ helm から置き換える程でない (helm-swoop の代替手段が存在しない等)
;; ;;; Vertico - ミニバッファを用いたファジーファインダー UI (Consult, Orderless, Marginalia と併用)
;; (use-package vertico
;;   :straight t
;;   :init
;;   (vertico-mode)
;;   )

;; ;;; Extensions/Vertico-directory - Vertico 拡張
;; (use-package extensions/vertico-directory
;;   :straight (:type built-in)
;;   :after vertico
;;   :ensure nil
;;   :bind (:map vertico-map
;;               ("C-l" . vertico-directory-up)
;;               ("\d" . vertico-directory-delete-char))
;;   )

;; ;;; Orderless - マッチ方法を変更し、スペース区切りで入力をマッチ
;; (use-package orderless
;;   :straight t
;;   :custom (completion-styles '(orderless))
;;   )

;; ;;; Marginalia - ミニバッファの補完に傍注（追加情報）を付与
;; (use-package marginalia
;;   :straight t
;;   :init
;;   (marginalia-mode)
;;   )

;;; Consult - コマンドの提供、候補リストの作成
;; consult-goto-line が便利なのでこれだけ利用
(use-package consult
  :straight t
  :bind (;; ("C-x b" . consult-buffer)      ; 文字化けするので helm を利用
         ("C-." . consult-goto-line))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (consult-project-root-function #'projectile-project-root)
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
;;; with-editor - Emacs から Git コミットメッセージを編集
(use-package with-editor
  :straight t
  :defer t)
;;; Git Gutter - ファイル内の変更点（追加・変更・削除）をサイドバーに表示
(use-package git-gutter
  :straight t
  :after with-editor
  :init
  (global-git-gutter-mode t)
  :custom
  (git-gutter:modified-sign "~")  ;; 変更
  (git-gutter:added-sign    "+")  ;; 追加
  (git-gutter:deleted-sign  "-")  ;; 削除
  :custom-face
  ;; (git-gutter:modified ((t (:background "#f1fa8c"))))
  ;; (git-gutter:added    ((t (:background "#50fa7b"))))
  ;; (git-gutter:deleted  ((t (:background "#ff79c6"))))
  )
;; :init/:config (global-git-gutter-mode t) が効かないので暫定対応
(add-hook 'emacs-startup-hook #'global-git-gutter-mode)

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
