;;;;; [Group] Library - ライブラリ関連 ;;;;;
;;; dash - Emacs 用のモダンなリスト操作ライブラリ
(use-package dash
  :ensure t
  )

;;; S - 文字列操作のための追加機能
(use-package s
  :ensure t
  )



;;;;; [Group] Themes-and-Visuals - テーマとビジュアル関連 ;;;;;
;;; Color Theme Modern - モダンなカラーテーマの設定。選択可能なテーマの幅を広げる
(use-package color-theme-modern
  :ensure t
  :config
  ;; テーマの適用
  ;; https://github.com/emacs-jp/replace-colorthemes/blob/master/screenshots.md
  (load-theme 'hober t t)
  (enable-theme 'hober)
  )

;;; Smart Mode Line - モードラインの外観と情報表示をカスタマイズ
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t
        sml/theme 'dark
        sml/shorten-directory -1)  ; ディレクトリパスはフル表示
  (sml/setup)
  )

;;; Hide Mode Line - 特定のモードでモードラインを隠す
(use-package hide-mode-line
  :ensure t
  :hook ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode)
  )



;;; Beacon - カーソルの位置を明確にするために点滅エフェクトを追加
(use-package beacon
  :ensure t
  :custom (beacon-color "yellow")
  :config (beacon-mode 1)
  )

;;; Volatile Highlights - 一時的なハイライト（選択範囲など）の強調表示
(use-package volatile-highlights
  :ensure t
  :diminish
  :hook (after-init . volatile-highlights-mode)
  :custom-face
  (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD"))))
  )

;;; Highlight Indent Guides - インデントレベルを視覚的に区別するためのガイド表示
(use-package highlight-indent-guides
  :ensure t
  :if (version< emacs-version "29") ; emacs29 で位置ズレが発生するようになるため無効化
  :diminish
  :hook (emacs-lisp-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)
  )

;;; Highlight Symbol - シンボルのハイライトとナビゲーション
(use-package highlight-symbol
  :ensure t
  :bind (([f3] . highlight-symbol-at-point)
         ([f4] . highlight-symbol-remove-all))
  :config
  (setq highlight-symbol-colors '("DeepSkyBlue1" "LimeGreen"  "HotPink1"      "Yellow"
                                  "Cyan"         "OrangeRed1" "MediumOrchid1" "SkyBlue"))
  )

;;; Rainbow-delimiters - 括弧の色分け
;; 括弧を色分けして対応関係を視覚的に表示する
(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode)
  :bind (("C-c l" . rainbow-delimiters-using-stronger-colors)) ; より強調した色に変更するコマンド
  :config
  ;; 括弧の色をより強調するための関数
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

;;; all-the-icons - icon setting
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  )



;;;;; [Group] Buffer-and-File-management - バッファとファイル管理関連 ;;;;;
;;; Xclip - クリップボードとの共有を可能にする
(use-package xclip
  ;; :disabled t
  :ensure t
  :init (xclip-mode 1) ; クリップボード共有を有効化
  )

;;; Dashboard - Emacs のスタートアップ画面をカスタマイズ
(use-package dashboard
  :ensure t
  :diminish (dashboard-mode page-break-lines-mode)
  :hook (after-init . dashboard-setup-startup-hook)
  )

;;; Total Lines - バッファ内の総行数をモードラインに表示
(use-package total-lines
  :ensure t
  :init (global-total-lines-mode t)
  :config
  (defun my-set-line-numbers ()
    "モードラインに全行数を表示。"
    (setq-default mode-line-front-space
                  (append mode-line-front-space
                          '((:eval (format " (%d)" (- total-lines 1))))))) ;; 「" (%d)"」の部分はお好みで
  (add-hook 'after-init-hook 'my-set-line-numbers)
  )

;;; Anzu - 検索や置換操作時にマッチ数や現在位置を表示
;; https://qiita.com/syohex/items/56cf3b7f7d9943f7a7ba
(use-package anzu
  :ensure t
  :init
  (global-anzu-mode 1)
  :config
  (setq anzu-mode-lighter ""
        anzu-deactivate-region t
        anzu-search-threshold 1000
        anzu-replace-to-string-separator " => ")
  )

;;; Recentf - 最近使用したファイルの履歴管理
(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  :config
  ;; メッセージを一時的に抑制するマクロ
  (defmacro with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))

  ;; recentf の設定
  (setq recentf-max-saved-items 2000                 ; 保存するファイルの数
        recentf-max-menu-items 15                    ; メニューに表示するアイテムの数
        recentf-exclude '("recentf-" user-full-name) ; 除外するファイルパターン（.recentf自体は含まない）
        recentf-auto-cleanup 'never                  ; 自動整理の設定
        ;; recentf の保存リストのパスをカスタマイズ (my-set-history@early-init.el)
        recentf-save-file (my-set-history "recentf-" user-full-name)
        ;; 30秒ごとに recentf リストを自動保存
        recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  )

;;; Recentf-ext - Recentfの拡張機能
(use-package recentf-ext
  :ensure t
  )

;;; Move Text - テキスト行の移動機能
(use-package move-text
  :ensure t
  :bind (("C-M-p" . move-text-up)
         ("C-M-n" . move-text-down))
  )

;;; Smooth-scroll - スムーズなスクロール操作（C-v など）
(use-package smooth-scroll
  :ensure t
  :config
  (smooth-scroll-mode t)                    ; smooth-scroll モードを有効化
  (setq smooth-scroll/vscroll-step-size 8)  ; 垂直スクロールのステップサイズを設定
  )



;;;;; [Group] Code-editting-and-Completion - コード編集と補完関連 ;;;;;
;;; Google C Style - Google の C スタイルガイドに準拠したコーディング
(use-package google-c-style
  :ensure t
  :hook ((c-mode-common c++-mode-common) . (lambda ()
                                             (google-set-c-style)
                                             (google-make-newline-indent)))
  )

;;; Aggressive Indent - コード編集時の自動インデント調整
(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  )

;;; Smart Newline - 改行時の自動インデントと位置調整
;; https://ainame.hateblo.jp/entry/2013/12/08/162032
(use-package smart-newline
  :ensure t
  :hook ((c++-mode c-mode cc-mode emacs-lisp-mode lisp-mode) . smart-newline-mode)
  :bind (("C-m" . smart-newline))
  )

;; Company - 自動補完機能の強化とカスタマイズ
(use-package company
  :ensure t
  :diminish company-mode
  :after company-statistics
  :init
  (global-company-mode 1)
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
         ;; 検索候補の移動を C-n と C-p で移動
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  ;; 基本設定
  (setq company-selection-wrap-around t
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance) ; 利用頻度が高いものを候補の上に表示
        company-idle-delay 0                                       ; デフォルトは 0.5
        company-show-numbers t
        company-tooltip-limit 10
        company-minimum-prefix-length 2                            ; デフォルトは 4
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-dabbrev-around t
        completion-ignore-case t
        company-dabbrev-downcase nil
        company-eclim-auto-save nil)
  ;; color settings（ツールチップの基本設定）
  (set-face-attribute 'company-tooltip nil
                      :foreground "white" :background "midnight blue")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "white" :background "midnight blue")
  ;; color settings（選択項目の設定）
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "dark slate blue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "white" :background "dark slate blue")
  ;; color settings（プレビューとスクロールバーの設定）
  (set-face-attribute 'company-preview-common nil
                      :background "dark slate blue" :foreground "white" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "dark gray")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "dim gray")
  ;; company-same-mode-buffers (プログラムの関数、変数のキーワード補完を強化) との統合
  ;; e.g. ファイル内のキーワードから補完
  (use-package company-same-mode-buffers
    :straight '(company-same-mode-buffers
                :type git
                :host github
                :repo "zk-phi/company-same-mode-buffers")
    :after company
    :ensure t
    :init
    (require 'company-same-mode-buffers)
    (company-same-mode-buffers-initialize)
    )
  ;; company-irony - Company と Irony の統合
  (use-package company-irony
    :ensure t
    :after (company irony)
    :config
    )
  ;; Company-irony-c-headers - C ヘッダファイル用の Company バックエンド
  (use-package company-irony-c-headers
    :ensure t
    :after (company irony)
    :config
    )
  ;; company-backends の設定 (yasnippet と company-same-mode-buffers は両立)
  (setq company-backends
        '((company-capf :with company-same-mode-buffers company-yasnippet)
          (company-dabbrev-code :with company-same-mode-buffers company-yasnippet)
          (company-irony-c-headers company-irony) ; Irony と Irony-C-Headers の組み合わせ
          company-keywords
          company-files
          company-dabbrev))
  )

;;; Company-statics - Company の表示順をスマートにする
(use-package company-statistics
  :ensure t
  :init
  (setq company-statistics-file
        (my-set-history "company-statistics-cache.el")) ; 履歴の保存場所 (@early-init.el)
  (company-statistics-mode)
  )

;;; Company-dwim - auto-complete に近い挙動で候補の絞り込みができる
(use-package company-dwim
  :straight '(company-dwim
              :type git
              :host github
              :repo "zk-phi/company-dwim")
  :ensure t
  :init
  (define-key company-active-map (kbd "TAB") 'company-dwim)
  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-dwim-frontend
          company-echo-metadata-frontend))
  )

;;; Company-anywhere - カーソルの位置がどこであっても company を起動できる
(use-package company-anywhere
  :straight '(company-anywhere
              :type git
              :host github
              :repo "zk-phi/company-anywhere")
  :ensure t
  )

;;; Company-box - Company の補完候補をパネル表示 (GUI 限定)
(use-package company-box
  :disabled t
  :ensure t
  :hook (company-mode . company-box-mode)
  )

;;; Company-posframe - Company の補完候補をパネル表示 (GUI 限定)
(use-package company-posframe
  :disabled t
  :ensure t
  :after company
  :diminish company-posframe-mode
  :config
  (company-posframe-mode 1)
  )

;;; Irony - C/C++ のコード補完とシンボル情報の提供
(use-package irony
  :ensure t
  :defer t
  :after cc-mode
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :init
  ;; Irony モードのインストール場所とオプションファイルの設定
  (setq irony-server-install-prefix    (expand-file-name "irony" my-history-dir))
  (setq irony-server-options-directory (expand-file-name "irony" my-history-dir))
  )

;;; Yasnippet - コードスニペットの管理と挿入
(use-package yasnippet
  :ensure t
  :diminish
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/my-data/snippets"
                           "~/.emacs.d/my-data/snippets/snippets" ;; シンボリックリンク用
                           "~/.emacs.d/loads/elisp/yasnippet-snippets-1.1/snippets"))
  :config
  (yas-global-mode 1)                             ; yasnippet のグローバルモードを有効化
  (use-package yasnippet-snippets :ensure t)      ; yasnippet の追加スニペット集
  (setq yas-prompt-functions '(yas-ido-prompt))   ; スニペット展開のプロンプト設定
  (custom-set-variables '(yas-trigger-key "TAB")) ; トリガーキーを TAB に設定

  ;; helm と yasnippet の統合
  (use-package helm-c-yasnippet
    :ensure t
    :diminish
    :bind (("C-c y" . helm-yas-complete))
    :config
    ;; helm のスペースマッチングを有効にする
    (setq helm-yas-space-match-any-greedy t)
    )
  )

;;; Copilot - Github copilot による補完
(use-package copilot
  :disabled t
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook ((prog-mode . copilot-mode))
  :init
  ;; Node.js の実行可能ファイルのパスを設定
  (setq copilot-node-executable "~/.nvm/versions/node/v21.6.1/bin/node")
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
  :config
  ;; *** Copilot の警告を抑止 ***
  ;; [Warning (copilot): ... copilot--infer-indentation-offset found no mode-specific indentation offset]
  ;; emacs-lisp-mode にて発生。copilot--indentation-alist への emacs-lisp-mode のダミー設定では抑制できず。
  ;; lisp-indent-offset への値設定にて抑制できるが、emacs-lisp-mode の思想に反し、
  ;; かつ aggresive-indent との競合が発生するため、警告抑止で対応する
  (setq warning-suppress-log-types '((copilot copilot-no-mode-indent)))
  (setq warning-suppress-types '((copilot copilot-no-mode-indent)))
  )

;;; Multiple Cursors - 複数カーソルによる編集機能
(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/list-file (my-set-history "mc-lists.el"))  ;; my-set-history@early-init.el 関数を使った設定

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
  (global-set-key (kbd "C-q") my/mc-repeat-map) ;; `C-q` をプレフィックスキーとして設定
  (repeat-mode 1) ;; `repeat-mode` を有効化
  )


;;; Expand Region - 選択範囲をインクリメンタルに拡大・縮小
(use-package expand-region
  :ensure t
  :init
  (transient-mark-mode t)
  :bind (("C-," . er/expand-region))
  )

;;; symbol-overlay - シンボルの置換
;; ※ Auto Highlight Symbol の ahs-edit-mode が Emacs 29 で正常に動作しないため置き換え
(use-package symbol-overlay
  :ensure t
  :hook (prog-mode . symbol-overlay-mode)
  :bind
  (("C-x C-a" . my-symbol-overlay-rename-visible)     ;; ウィンドウ内のシンボルを置換
   ("C-x a"   . my-symbol-overlay-rename-in-function) ;; 関数・メソッド内の置換
   ("C-x C-g" . symbol-overlay-rename)                ;; バッファ全体のシンボルを置換
   )
  :config
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



;;;;; [Group] Org - Org 関係 ;;;;;
;;; org - org 設定
(use-package org
  :ensure t
  :defer t
  :hook ((org-mode . visual-line-mode))  ;; 自動改行の有効化
  :custom
  (org-hide-leading-stars t)             ;; 見出しの*を非表示
  (org-startup-indented t)               ;; インデント表示をデフォルトで有効化
  (org-src-fontify-natively t)           ;; ソースコードをシンタックスハイライト
  (org-src-tab-acts-natively t)          ;; org-babelでタブキーを言語モードに連動
  (org-edit-src-content-indentation 2)   ;; org-babelのソースコードインデント
  (org-startup-folded 'content)          ;; 初期表示で折りたたむ
  (org-log-done 'time)                   ;; タスク完了時に時間を記録
  (org-log-into-drawer t)                ;; ログを :LOGBOOK: に格納
  (org-adapt-indentation nil)            ;; インデントの自動調整をオフにする
  (org-cycle-separator-lines 2)          ;; 見出しの間隔
  (org-ellipsis " ▼")                   ;; 折りたたみ表示の記号変更
  :config
  (setq org-agenda-files '("~/org/agenda/"))  ;; アジェンダファイルのディレクトリ指定
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  )

;;; org-indent - インデントを自動調整
(use-package org-indent
  :ensure nil
  :hook (org-mode . org-indent-mode)
  )

;;; org-modern - 全体的なUI向上 (*) org-indent-mode と相性が悪いので一旦無効化
(use-package org-modern
  :disabled t
  :ensure t
  :after org
  :hook (org-mode . org-modern-mode)
  )

;;; org-download - 画像のクリップボード貼り付け
(use-package org-download
  :ensure t
  :after org
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
  :ensure nil
  :after org
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
  :ensure nil
  :after org
  :bind ("C-c a" . org-agenda)
  :custom
  (setq org-agenda-files (file-expand-wildcards (concat org-directory "/*.org")))
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
(defvar my-markdown-css-file (expand-file-name "~/.emacs.d/my-data/css/markdown-style.css")
  "Path to custom markdown CSS file.")

;; Pandoc コマンドの設定
;; その他 option: "--toc --toc-depth=2", "--highlight-style=tango"
(defvar my-markdown-pandoc-command
  (concat "pandoc -s --number-sections --self-contained -f markdown -t html5"
          " --css " my-markdown-css-file)
  "Pandoc command for Markdown export.")

;; CSS を HTML に埋め込むために、CSS の内容を変数に格納
(defvar my-markdown-css-content
  (with-temp-buffer
    (insert-file-contents my-markdown-css-file)
    (buffer-string))
  "Content of the markdown CSS file.")

;;; markdown-mode - markdown mode の設定
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :hook ((markdown-mode . flyspell-mode)                     ;; スペルチェック
         (markdown-mode . visual-line-mode)                  ;; 行の折り返し
         (markdown-mode . (lambda ()                         ;; markdown-indent-on-enter
                            (setq markdown-indent-on-enter t)))
         (markdown-mode . display-line-numbers-mode)         ;; 行番号表示
         (markdown-mode . outline-minor-mode)                ;; 見出しの折りたたみ
         (markdown-mode . (lambda ()
                            (setq markdown-enable-math t)    ;; 数式を有効化
                            (setq markdown-fontify-code-blocks-natively t) ;; コードブロックの色付け
                            (electric-pair-mode t)))         ;; 括弧の自動補完
         )
  ;; インデント設定
  :custom
  (markdown-indent-level 2)
  (markdown-link-space-substitution-method 'underscores) ;; リンクのスペースをアンダースコアに置換
  (markdown-header-scaling t)                            ;; 見出しサイズの自動調整
  ;; markdown コマンドを pandoc に置き換え
  (markdown-command my-markdown-pandoc-command)
  (markdown-export-command my-markdown-pandoc-command)
  (markdown-xhtml-header-content
   (format "<meta charset='utf-8'>\n
            <meta name='viewport' content='width=device-width, initial-scale=1'>\n
            <title>Markdown Export</title>\n
            <style>\n%s\n</style>\n
            <script src='https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.0/highlight.min.js'></script>\n
            <script>hljs.configure({languages: []});hljs.highlightAll();</script>\n"
           my-markdown-css-content))
  :bind (("C-c C-v h" . markdown-insert-header-dwim)     ;; 見出しを挿入
         ("C-c C-v l" . markdown-insert-link)            ;; リンクを挿入
         ("C-c C-v c" . markdown-insert-gfm-code-block)  ;; コードブロックを挿入
         ("C-c C-v d" . markdown-insert-details)         ;; 折り畳み項目を挿入
         )
  :config
  ;; コードブロックのシンタックスハイライト
  (setq markdown-code-lang-modes
        '(("bash"   . shell-script)
          ("elisp"  . emacs-lisp)
          ("python" . python))
        )
  ;; プレビュー機能
  (add-hook 'markdown-mode-hook 'markdown-preview-mode)
  ;; 画像を表示する設定（ImageMagick がインストールされていることが前提）
  (setq markdown-image-use-cache t)
  (add-hook 'markdown-mode-hook #'turn-off-auto-fill)
  (add-hook 'markdown-mode-hook #'turn-on-visual-line-mode)
  ;; 折りたたみ関数
  (defun markdown-insert-details ()
    "Insert <details> HTML tag with a <summary>."
    (interactive)
    (insert "<details><summary>text</summary><div>\n\n</div></details>"))
  )

;;; markdown-toc - 目次生成
(use-package markdown-toc
  :ensure t
  :after markdown-mode
  :hook ((markdown-mode . markdown-toc-mode)       ;; テーブルの自動整形
         ;; (markdown-mode . markdown-toc-insert-toc) ;; 保存時に目次を自動挿入
         ;; (before-save . markdown-toc-update-toc)   ;; 保存前に目次を更新
         )
  :bind (("C-c C-v t" . markdown-toc-generate-toc))  ;; 目次を手動で生成
  :config
  (setq markdown-toc-without-markdown-toc t)  ;; コメントを含めないようにする
  (setq markdown-toc-headline-style 'atx)     ;; 見出しのスタイル
  )

;;; pandoc-mode - フォーマット変換を簡易に
(use-package pandoc-mode
  :ensure t
  :commands (markdown-mode pandoc-mode)
  )



;;;;; [Group] Navigation-and-Search - ナビゲーションと検索関連 ;;;;;
;;; Popwin - ポップアップウィンドウの管理
(use-package popwin
  :ensure t
  :custom
  (popwin:popup-window-position 'bottom)
  :config
  (popwin-mode 1)
  )

;;; Helm - 効率的なバッファやコマンドの検索
(use-package helm
  :ensure t
  :diminish
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
  :config
  (setq helm-display-function #'display-buffer       ; Helm の表示に 'display-buffer' 関数を使用
        helm-echo-input-in-header-line t             ; 入力を Helm のヘッダーラインに表示
        helm-input-idle-delay 0.2                    ; 入力から画面の更新までの遅延時間（デフォルトは 0.01 秒）
        helm-split-window-inside-p t                 ; Helm のウィンドウを現在のウィンドウ内に分割して表示
        helm-move-to-line-cycle-in-source t          ; Helm バッファ内で候補の最後に到達したら先頭に戻る
        helm-ff-file-name-history-use-recentf t      ; 'recentf' リストをファイル名の履歴として使用
        helm-ff-search-library-in-sexp t             ; SEXP 内でライブラリの検索を有効化
        helm-ff-fuzzy-matching nil                   ; ファジーマッチングを無効化
        helm-buffer-details-flag nil                 ; バッファの詳細情報を非表示
        helm-delete-minibuffer-contents-from-point t ; ミニバッファの内容を現在のポイントから削除
        )
  ;; Emacsのコマンドと履歴のソース定義
  (defvar helm-source-emacs-commands
    (helm-build-sync-source "Emacs commands"
      :candidates (lambda ()
                    (let ((cmds))
                      (mapatoms (lambda (elt) (when (commandp elt) (push elt cmds))))
                      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "A simple helm source for Emacs commands.")
  (defvar helm-source-emacs-commands-history
    (helm-build-sync-source "Emacs commands history"
      :candidates (lambda ()
                    (let ((cmds))
                      (dolist (elem extended-command-history)
                        (push (intern elem) cmds))
                      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "Emacs commands history")
  ;; 検索パターンの変換
  (defun helm-buffers-list-pattern-transformer (pattern)
    (if (equal pattern "")
        pattern
      (let* ((first-char (substring pattern 0 1))
             (pattern (cond ( (equal first-char "*") (concat " " pattern))
                            ((equal first-char "=") (concat "*" (substring pattern 1)))
                            (t pattern))))
        (setq pattern (replace-regexp-in-string "\\." "\\\\." pattern))
        (setq pattern (replace-regexp-in-string "\\*" "\\\\*" pattern))
        pattern)))

  ;; Emulate `kill-line' in helm minibuffer
  (defadvice helm-delete-minibuffer-contents (before emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-file-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff--transform-pattern-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
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
                              ;; Do not use `source' because other plugins
                              ;; (such as helm-migemo) may change it
                              (helm-candidates-in-buffer (helm-get-current-source)))))
                    (volatile) (match identity)))
        source))
    (defalias 'helm-mp-3-get-patterns 'helm-mm-3-get-patterns)
    (defalias 'helm-mp-3-search-base 'helm-mm-3-search-base))
  )

;;; Helm GTags - ソースコード内のシンボル検索とナビゲーション
(use-package helm-gtags
  :ensure t
  :diminish
  :hook ((c-mode   . helm-gtags-mode)
         (c++-mode . helm-gtags-mode))
  ;; グローバルにバインドする方針に変更
  :bind (
         :map helm-gtags-mode-map
         ("C-t d"   . helm-gtags-find-tag)         ; 関数の定義場所の検索 (define)（旧 f11）
         ("C-t C-d" . helm-gtags-find-tag)         ; 関数の定義場所の検索 (define)
         ("C-t u"   . helm-gtags-find-rtag)        ; 関数の使用箇所の検索 (use)（旧 f12）
         ("C-t C-u" . helm-gtags-find-rtag)        ; 関数の使用箇所の検索 (use)
         ("C-t v"   . helm-gtags-find-symbol)      ; 変数の使用箇所の検索 (valiable)（旧 f9）
         ("C-t C-v" . helm-gtags-find-symbol)      ; 変数の使用箇所の検索 (valiable)
         ("C-t f"   . helm-gtags-find-files)       ; ファイルジャンプ     (find)（旧 f6）
         ("C-t C-f" . helm-gtags-find-files)       ; ファイルジャンプ     (find)
         ;; ("C-t")    . helm-gtags-pop-stack)     ; 前のバッファへ→previous-history に移管
         ("C-t p"   . helm-gtags-previous-history) ; 前のバッファへ       (perv)
         ("C-t C-p" . helm-gtags-previous-history) ; 前のバッファへ       (perv)
         ("C-t n"   . helm-gtags-next-history)     ; 次のバッファへ       (next)
         ("C-t C-n" . helm-gtags-next-history)     ; 次のバッファへ       (next)
         )
  :config
  (custom-set-variables '(helm-gtags-path-style 'root))
  ;; GTAGSの自動更新関数
  (defun update-gtags ()
    (interactive)
    (when (and (buffer-file-name)
               (executable-find "global"))
      (start-process "gtags-update" nil "global" "-uv")))
  ;; 必要に応じてアンコメントして使用
  ;; (add-hook 'after-save-hook 'update-gtags)
  )

;;; Helm Descbinds - コマンドとキーバインドの検索（helm-M-x でキーバインドを表示）
(use-package helm-descbinds
  :ensure t
  :after helm
  :init (helm-descbinds-mode 1)
  )

;;; Helm AG - ファイルの内容を高速検索
(use-package helm-ag
  :ensure t
  )

;;; Helm-swoop - インクリメンタルサーチ機能の強化
(use-package helm-swoop
  :ensure t
  :bind (("C-x s" . helm-swoop))
  :config
  ;; 検索行のハイライト色を設定（例：明るい青の背景と白のテキスト）
  (set-face-attribute 'helm-swoop-target-line-face nil
                      :background "#0077ff" ; 明るい青
                      :foreground "white")

  ;; 検索語のハイライト色を設定（例：明るい緑の背景と黒のテキスト）
  (set-face-attribute 'helm-swoop-target-word-face nil
                      :background "#00ff00" ; 明るい緑
                      :foreground "black")
  )

;;; Swiper - インクリメンタルサーチ機能の強化
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper))
  )

;;; Ivy - 効率的なバッファやファイルの検索 (swiper の強化)
(use-package ivy
  :ensure t
  :after ivy-migemo
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-height 30  ;; minibufferのサイズを拡大
        ivy-extra-directories nil)
  ;; swiper と連携
  ;; https://github.com/ROCKTAKEY/ivy-migemo
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)
                                (swiper . ivy-migemo--regex-plus)
                                ;; (t . ivy--regex-fuzzy)             ; fuzzy バージョン
                                ;; (swiper . ivy-migemo--regex-fuzzy) ; fuzzy バージョン
                                ))
  )

;;; Ivy-migemo - Ivy で Migemo を利用 (Swiper/Ivy の強化)
(use-package ivy-migemo
  :ensure t
  )

;;; Migemo - 日本語を含む検索時の挙動改善
;; 参考: http://rubikitch.com/2014/08/20/migemo/
(use-package migemo
  :ensure t
  :config
  ;; Set your installed path
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
;;   :ensure t
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
;;   :ensure t
;;   :custom (completion-styles '(orderless))
;;   )

;; ;;; Marginalia - ミニバッファの補完に傍注（追加情報）を付与
;; (use-package marginalia
;;   :ensure t
;;   :init
;;   (marginalia-mode)
;;   )

;;; Consult - コマンドの提供、候補リストの作成
;; consult-goto-line が便利なのでこれだけ利用
(use-package consult
  :ensure t
  :bind (;; ("C-x b" . consult-buffer)      ; 文字化けするので helm を利用
         ("C-." . consult-goto-line))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (global-set-key [remap goto-line] 'consult-goto-line) ; goto-line@00-key-binding を置き換え
  (setq consult-project-root-function #'projectile-project-root)
  )

;;; Neotree - ファイルツリー表示とナビゲーション
(use-package neotree
  :ensure t
  :after projectfile
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  ;; :bind (([f8] . neotree-toggle)) ; うまくバインドできない
  :preface
  (bind-key [f8] 'neotree-toggle)
  (defun neotree-toggle ()
    (interactive)
    (let ((project-dir (ignore-errors (projectile-project-root)))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir (neotree-dir project-dir))
          (if file-name   (neotree-find file-name)))))
    )
  )

;;; Imenu List - バッファ内のシンボルリスト表示
(use-package imenu-list
  :ensure t
  )



;;;;; [Group] Languages-and-Style - 言語とスタイル関連 ;;;;;
;;; Mozc - 日本語入力の設定
(use-package mozc
  :ensure t
  :config (prefer-coding-system 'utf-8)
  )

;;; ispell - スペルチェック機能の設定と辞書の指定（flyspell のバックエンド）
(use-package ispell
  :ensure t
  :config
  (cond
   ;; hunspell があれば優先
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "en_US")
    (setq ispell-extra-args '("-d" "en_US"))) ;; hunspell に適したオプション
   ;; aspell があれば使用
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-dictionary "en_US")
    (setq ispell-extra-args '("--sug-mode=ultra"))))
  (setq ispell-silently-savep t) ;; ユーザー辞書の保存時に確認しない
  (setq ispell-skip-region-alist '(("[^\000-\377]+"))) ;; 日本語無視
  )

;;; flyspell - リアルタイムスペルチェック機能（フロントエンド）
(use-package flyspell
  :ensure t
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode)                    ;; Emacs Lisp を除外
                          (setq-local ispell-skip-region-alist '(("[^\000-\377]+"))) ;; 日本語無視
                          (flyspell-prog-mode))))
         ((text-mode html-mode markdown-mode) . (lambda () (flyspell-mode -1)))      ;; 無効化
         (find-file . (lambda ()
                        (when (> (buffer-size) 3000) ;; 3000行以上なら無効
                          (flyspell-mode -1))))
         )
  :bind (:map flyspell-mode-map
              ("C-," . nil)
              ("C-." . nil)
              ("C-;" . nil)
              ("C-c $" . nil))
  :config
  (setq flyspell-issue-message-flag nil) ;; ミニバッファメッセージ抑制
  )

;;; flyspell-correct - スペルチェックの補助ツール
(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c C-/" . flyspell-correct-wrapper)) ;; C-/ で補正メニューを開く
  )

;;; flyspell-correct-popup - pop-up メニューで修正候補を選べるようにする
(use-package flyspell-correct-popup
  :ensure t
  :after flyspell-correct
  :custom
  (flyspell-correct-interface #'flyspell-correct-popup)
  )



;;;;; [Group] Version-control - バージョン管理関連 ;;;;;
;;; Git Gutter - ファイル内の変更点（追加・変更・削除）をサイドバーに表示
(use-package with-editor
  :ensure t
  :defer t)
(use-package git-gutter
  :ensure t
  :after with-editor dash
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  :custom-face
  ;; (git-gutter:modified ((t (:background "#f1fa8c"))))
  ;; (git-gutter:added    ((t (:background "#50fa7b"))))
  ;; (git-gutter:deleted  ((t (:background "#ff79c6"))))
  :config
  (global-git-gutter-mode +1)
  )

;;; Dsvn - SVN 管理ツール
(use-package dsvn
  :ensure t
  )



;;;;; [Group] Misc-utilities - その他のユーティリティ ;;;;;
;;; Paradox - パッケージのインストールと更新
(use-package paradox
  :ensure t
  :config
  (paradox-enable)
  (setq paradox-github-token t)
  )

;;; Free-keys - 未使用のキーバインドを表示
(use-package free-keys
  :ensure t
  )

;;; undo-fu - undo と redo を強化
(use-package undo-fu
  :ensure t
  :config
  (with-eval-after-load 'evil
    (setq evil-undo-system 'undo-fu))
  )

;;; undo-fu-session - undo 情報を Emacs 終了後も保持
(use-package undo-fu-session
  :ensure t
  :config
  (setq undo-fu-session-directory (my-set-history "undo-fu-session/"))
  (unless (file-exists-p undo-fu-session-directory)
    (make-directory undo-fu-session-directory t))
  (undo-fu-session-global-mode +1)
  )

;;; Vundo - アンドゥの操作のツリー表示と管理
(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-compact-display nil) ;; 画面を広めに使う
  (setq vundo-window-max-height 8) ;; `vundo` のウィンドウ高さを大きくする
  (setq vundo-roll-back-on-quit t) ;; `q` で抜けた時に元の位置に戻る
  :init
  (with-eval-after-load 'vundo
    (define-key vundo-mode-map (kbd "C-f") 'vundo-forward)   ;; 次の状態へ (→)
    (define-key vundo-mode-map (kbd "C-b") 'vundo-backward)  ;; 前の状態へ (←)
    (define-key vundo-mode-map (kbd "C-n") 'vundo-next)      ;; 下の分岐へ (↓)
    (define-key vundo-mode-map (kbd "C-p") 'vundo-previous)) ;; 上の分岐へ (↑)
  )

;;; Undohist - アンドゥ履歴のファイル保存
(use-package undohist
  :ensure t
  :diminish  ; モードラインの表示を隠す
  :config
  (setq undohist-directory (my-set-history "undohist")) ; アンドゥ履歴の保存場所 (@early-init.el)
  (undohist-initialize)                                 ; undohist を初期化
  )

;;; Which-key - 使用可能なキーバインドの表示
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :custom
  (which-key-max-description-length 40)  ; 説明の最大長
  (which-key-use-C-h-commands t)         ; C-h コマンドを使用
  :hook
  (after-init . which-key-mode)          ; Emacs 起動後に which-key モードを有効化
  )

;;; Amx - M-x コマンドの強化（コマンド履歴などを改善）
(use-package amx
  :ensure t
  :config
  (setq amx-save-file (my-set-history "amx-items")) ; Amx の履歴ファイルの保存場所 (@early-init.el)
  )

;;; Stopwatch
(use-package stopwatch
  :straight (stopwatch
             :type git
             :host github
             :repo "blue0513/stopwatch") ; https://github.com/blue0513/stopwatch
  :ensure t
  )

;;; Initchart - 初期化処理の実行時間を記録
(use-package initchart
  :straight (initchart
             :type git
             :host github
             :repo "yuttie/initchart") ; https://github.com/yuttie/initchart
  :ensure t
  :config
  (initchart-record-execution-time-of load file)
  (initchart-record-execution-time-of require feature)
  )
