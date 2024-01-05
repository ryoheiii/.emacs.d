;;; Color Theme Modern - モダンなカラーテーマの設定。選択可能なテーマの幅を広げる
(use-package color-theme-modern
  :ensure t
  :config
  ;; カスタムテーマのロードパスを追加
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory "~/.emacs.d/loads/elisp/color-theme-modern-0.0.3"))
  ;; テーマの適用
  ;; https://github.com/emacs-jp/replace-colorthemes/blob/master/screenshots.md
  (load-theme 'hober t t)
  (enable-theme 'hober)
  )

;;; Xclip - クリップボードとの共有を可能にする
(use-package xclip
  :ensure t
  :init (xclip-mode 1) ; クリップボード共有を有効化
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

;;; Hide Mode Line - 特定のモードでモードラインを隠す
(use-package hide-mode-line
  :ensure t
  :hook ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode)
  )

;;; Full Column Indicator - テキストの折り返し位置を視覚的に示す
(use-package fill-column-indicator
  :ensure t
  :hook ((markdown-mode git-commit-mode) . fci-mode)
  )

;;; Git Gutter+ - ファイル内の変更点（追加・変更・削除）をサイドバーに表示
;; dash - Emacs 用のモダンなリスト操作ライブラリ (git-gutter+ の依存パッケージ)
(use-package dash :ensure t)
(use-package git-gutter+
  :ensure t
  :after dash
  :custom
  (git-gutter+:modified-sign "~")
  (git-gutter+:added-sign    "+")
  (git-gutter+:deleted-sign  "-")
  :custom-face
  (git-gutter+:modified ((t (:background "#f1fa8c"))))
  (git-gutter+:added    ((t (:background "#50fa7b"))))
  (git-gutter+:deleted  ((t (:background "#ff79c6"))))
  :config
  (global-git-gutter+-mode +1)
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
  :diminish
  :hook (emacs-lisp-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)
  )

;;; Aggressive Indent - コード編集時の自動インデント調整
(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  )

;;; Dashboard - Emacs のスタートアップ画面をカスタマイズ
(use-package dashboard
  :ensure t
  :diminish (dashboard-mode page-break-lines-mode)
  :hook (after-init . dashboard-setup-startup-hook)
  )

;;; Smart Newline - 改行時の自動インデントと位置調整
;; https://ainame.hateblo.jp/entry/2013/12/08/162032
(use-package smart-newline
  :ensure t
  :init
  (dolist (mode '(c++-mode-hook c-mode-hook cc-mode-hook emacs-lisp-mode-hook lisp-mode-hook))
    (add-hook mode (lambda () (smart-newline-mode 1))))
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

;;; Ispell - スペルチェック機能の設定と辞書の指定
(use-package ispell
  :ensure t
  :config
  ;; aspell にパスを設定
  (when (file-executable-p "/usr/bin/aspell")
    (setq-default ispell-program-name "aspell")
    (add-to-list 'ispell-extra-args "--sug-mode=ultra"))
  ;; 日本語をスキップする設定
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  ;; スペルチェックに英語の辞書を使用
  (setq ispell-dictionary "american")
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
  :config
  ;; Irony と Company の統合
  (use-package company-irony-c-headers
    :ensure t
    :config
    ;; Company バックエンドに Irony の補完を追加
    (add-to-list 'company-backends '(company-irony-c-headers company-irony)))
  )

;; Company - 自動補完機能の強化とカスタマイズ
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode 1)
  :bind (("C-M-i" . company-complete)
         :map company-mode-map
         ("TAB" . indent-for-tab-command)
         :map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("TAB" . company-complete-selection)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-selection-wrap-around t
        company-transformers '(company-sort-by-backend-importance) ; ソート順
        company-idle-delay 0                                       ; デフォルトは 0.5
        company-show-numbers t
        company-tooltip-limit 10
        company-minimum-prefix-length 3                            ; デフォルトは 4
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-dabbrev-around t
        completion-ignore-case t
        company-dabbrev-downcase nil
        company-eclim-auto-save nil)
  ;; Yasnippetとの連携
  (defvar company-mode/enable-yas t)
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  ;; color settings
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "grey60")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40")
  )

;;; Auto-complete - コード補完機能の提供と設定（company に置き換え）
(use-package auto-complete
  :ensure t
  :demand t
  :diminish ""
  :bind (:map ac-menu-map
              ("C-n" . ac-next)
              ("C-p" . ac-previous)
              )
  :config
  (use-package auto-complete-c-headers
    :ensure t
    :init
    (add-hook 'c++-mode-hook (lambda ()
                               '(setq ac-sources (append ac-sources '(ac-source-c-headers)))))
    (add-hook 'c-mode-hook (lambda ()
                             '(setq ac-sources (append ac-sources '(ac-source-c-headers)))))
    )
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/my-data/ac-dict")
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/my-data/ac-dict/ac-dict") ;; for symbolic link
  (add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
  (ac-set-trigger-key "TAB")
  (setq ac-comphist-file (my-set-history "ac-comphist.dat")) ;; my-set-history @00-auto-file-place.el
  (setq ac-use-menu-map t
        ac-disable-faces nil ;コメントや文字列リテラルでも補完を行う
        ac-use-fuzzy t)      ; 曖昧マッチ
  ;; yasnippet の binding を指定するとエラーが出るので回避する方法。
  (setf (symbol-function 'yas-active-keys)
        (lambda ()
          (remove-duplicates (mapcan #'yas--table-all-keys (yas--get-snippet-tables)))))
  )

;;; ElScreen - 複数のウィンドウ（スクリーン）を管理
;; キー
;; C-z c    新規スクリーンを作成して移動する elscreen-create
;; C-z k    現在のスクリーンを閉じる         elscreen-kill
;; C-z p    前のスクリーンへ                 elscreen-previous
;; C-z n    次のスクリーンへ                 elscreen-next
;; C-z a    前と次のスクリーンをトグル       elscreen-toggle
;; C-z [0-9]    番号のスクリーンへ           elscreen-jump-[0-9]
;; C-z ?    ヘルプを表示する

;; helm 連携キー
;; C-z C-f  新しいelscreenでファイルを開く
;; C-z b    新しいelscreenでバッファを開く
;; C-z d    新しいelscreenでdiredを開く
(use-package elscreen
  :ensure t
  :config
  ;; Elscreenのプレフィックスキーを設定
  (setq elscreen-prefix-key (kbd "C-z"))
  ;; Elscreenを起動
  (elscreen-start)
  ;; タブの表示設定
  (setq elscreen-tab-display-kill-screen nil  ; タブの先頭に[X]を表示しない
        elscreen-tab-display-control nil)    ; header-lineの先頭に[<->]を表示しない
  ;; バッファ名・モード名に基づくタブのニックネーム設定
  (setq elscreen-buffer-to-nickname-alist
        '(( "^dired-mode$" . (lambda () (format "Dired(%s)" dired-directory)))
          ("^Info-mode$"  . (lambda () (format "Info(%s)" (file-name-nondirectory Info-current-file))))
          ("^mew-"        . "Mew")
          ("^irchat-"     . "IRChat")
          ("^liece-"      . "Liece")
          ("^lookup-"     . "Lookup"))
        elscreen-mode-to-nickname-alist
        '(( "[Ss]hell"     . "shell")
          ("compilation"  . "compile")
          ("-telnet"      . "telnet")
          ("dict"         . "OnlineDict")
          ("*WL:Message*" . "Wanderlust")))
  ;; フレームタイトルにタブ情報を表示
  (setq *elscreen-tab-truncate-length* 20)
  (defun elscreen-tabs-as-string ()
    "Return a string representation of elscreen tab names."
    (let* ((screen-list (sort (elscreen-get-screen-list) '<))
           (screen-to-name-alist (elscreen-get-screen-to-name-alist)))
      (mapconcat
       (lambda (screen)
         (format (if (string-equal "+" (elscreen-status-label screen))
                     "[ %d ] %s"
                   "(%d) %s")
                 screen
                 (elscreen-truncate-screen-name
                  (alist-get screen screen-to-name-alist) *elscreen-tab-truncate-length*)))
       screen-list " | ")))
  (defvar *elscreen-tabs-as-string* "" "Variable to hold current elscreen tab names as a string.")
  (defun update-elscreen-tabs-as-string ()
    "Update *elscreen-tabs-as-string* variable."
    (interactive)
    (setq *elscreen-tabs-as-string* (elscreen-tabs-as-string)))
  (add-hook 'elscreen-screen-update-hook 'update-elscreen-tabs-as-string)
  (setq frame-title-format '(:eval (concat *elscreen-tabs-as-string*
                                           "    ||    "
                                           (if buffer-file-name
                                               (abbreviate-file-name buffer-file-name) "%b"))))
  )

;;;Expand Region - 選択範囲をインクリメンタルに拡大・縮小
(use-package expand-region
  :ensure t
  :init
  (transient-mark-mode t)
  :bind (("C-," . er/expand-region)
         ;; ("C-M-," . er/contract-region)
         )
  )

;;; Google C Style - Google の C スタイルガイドに準拠したコーディング
(use-package google-c-style
  :ensure t
  :hook (( c-mode-common   . (lambda ()
                               (google-set-c-style)
                               (google-make-newline-indent)))
         (c++-mode-common . (lambda ()
                              (google-set-c-style)
                              (google-make-newline-indent))))
  )

;;; Helm - 効率的なバッファやコマンドの検索
(use-package helm
  :ensure t
  :diminish
  :init
  (setq helm-ff-file-name-history-use-recentf t
        helm-display-function #'display-buffer)
  (helm-mode 1)
  :bind (( "M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x C-y" . helm-show-kill-ring)
         ("C-x g"   . helm-do-grep-ag)
         ("C-x b"   . helm-buffers-list)
         ("C-x i"   . helm-imenu)
         :map helm-map
         ("C-h" . delete-backward-char)
         :map helm-find-files-map
         ("C-h" . delete-backward-char)
         ("TAB" . helm-execute-persistent-action)
         :map helm-read-file-map
         ("TAB" . helm-execute-persistent-action))
  :config
  (setq helm-buffer-details-flag nil
        helm-delete-minibuffer-contents-from-point t
        helm-ff-fuzzy-matching nil)
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
  )

;;; Helm GTags - ソースコード内のシンボル検索とナビゲーション
(use-package helm-gtags
  :ensure t
  :diminish
  :hook (( c-mode   . helm-gtags-mode)
         (c++-mode . helm-gtags-mode))
  :bind (( [f11] . helm-gtags-find-tag)    ;; 関数の定義場所の検索
         ([f12] . helm-gtags-find-rtag)   ;; 関数の使用箇所の検索
         ([f9]  . helm-gtags-find-symbol) ;; 変数の使用箇所の検索
         ("C-t" . helm-gtags-pop-stack)   ;; gtagsでジャンプする一つ前の状態に戻る
         ([f6]  . helm-gtags-find-files)  ;; ファイルジャンプ
         )
  :config
  (custom-set-variables
   '(helm-gtags-path-style 'root))
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
  :init (helm-descbinds-mode 1)
  )

;;; Helm AG - ファイルの内容を高速検索
(use-package helm-ag
  :ensure t
  )

;;; Highlight Symbol - シンボルのハイライトとナビゲーション
(use-package highlight-symbol
  :ensure t
  :bind (( [f3] . highlight-symbol-at-point)
         ([f4] . highlight-symbol-remove-all))
  :config
  (setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))
  )

;;; Auto Highlight Symbol の設定
(use-package auto-highlight-symbol
  :ensure t
  :config
  (global-auto-highlight-symbol-mode t)
  ;; C-x C-aで一括rename
  )

;;; Imenu List - バッファ内のシンボルリスト表示
(use-package imenu-list
  :ensure t
  )

;;; Ivy - 効率的なバッファやファイルの検索
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-height 30  ;; minibufferのサイズを拡大
        ivy-extra-directories nil
        ivy-re-builders-alist '((t . ivy--regex-plus)))
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
  ;; Helmとの統合設定
  (use-package helm
    :init (helm-migemo-mode 1)
    )
  )

;;; Avy-Migemo - キーボードナビゲーションの強化と日本語対応
(use-package avy-migemo
  :ensure t
  :init (avy-migemo-mode 1)
  :config
  (setq avy-timeout-seconds nil)
  (global-set-key (kbd "C-M-;") 'avy-migemo-goto-char-timer)
  ;; 以下の行は必要に応じてアンコメントして使用
  ;; (use-package avy-migemo-e.g.swiper :ensure t)
  ;; (global-set-key (kbd "M-g m m") 'avy-migemo-mode)
  )

;;; Move Text - テキスト行の移動機能
(use-package move-text
  :ensure t
  :bind (( "C-M-p" . move-text-up)
         ("C-M-n" . move-text-down))
  )

;;; Mozc - 日本語入力の設定
(use-package mozc
  :ensure t
  :config (prefer-coding-system 'utf-8)
  )

;;; Codic - プログラミング用語の翻訳と検索
(use-package codic
  :ensure t
  )

;;; Multiple Cursors - 複数カーソルによる編集機能
(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/list-file (my-set-history "mc-lists.el"))  ;; my-set-history@00-auto-file-place.el 関数を使った設定

  (global-set-key (kbd "C-M-c") 'mc/edit-lines)
  (global-set-key (kbd "C-*")   'mc/mark-all-like-this)

  ;; Smartrepの設定
  (use-package smartrep
    :ensure t
    :config
    (declare-function smartrep-define-key "smartrep")
    ;; C-q をプレフィックスキーとして設定
    (global-set-key (kbd "C-q") nil)
    (smartrep-define-key global-map "C-q"
      '(( "p" . mc/mark-previous-like-this)
        ("n" . mc/mark-next-like-this)
        ("*" . mc/mark-all-like-this)
        ("d" . mc/mark-all-like-this-dwim)
        ("m" . mc/mark-more-like-this-extended)
        ("u" . mc/unmark-next-like-this)
        ("U" . mc/unmark-previous-like-this)
        ("s" . mc/skip-to-next-like-this)
        ("S" . mc/skip-to-previous-like-this)
        ("i" . mc/insert-numbers)
        ("o" . mc/sort-regions)
        ("O" . mc/reverse-regions)))

    ;; smartrepによるコマンド実行中のキー入力エコーを無効にする
    ;; http://shakenbu.org/yanagi/d/?date=20140105
    (advice-add 'smartrep-map-internal
                :around (lambda (orig-fun &rest args)
                          (let ((echo-keystrokes 0))
                            (apply orig-fun args))))
    )
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
  ;; (setq recentf-save-file "~/.emacs.d/hist/recentf" "-" user-full-name)
  (setq recentf-max-saved-items 2000                 ; 保存するファイルの数
        recentf-max-menu-items 15                    ; メニューに表示するアイテムの数
        recentf-exclude '("recentf-" user-full-name) ; 除外するファイルパターン（.recentf自体は含まない）
        recentf-auto-cleanup 'never                  ; 自動整理の設定
        ;; recentf の保存リストのパスをカスタマイズ (my-set-history@00-auto-file-place.el)
	recentf-save-file (my-set-history "recentf-" user-full-name)
        ;; 30秒ごとに recentf リストを自動保存
        recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  )

;;; Recentf-ext - Recentfの拡張機能
(use-package recentf-ext
  :ensure t
  )

;;; Smooth-scroll - スムーズなスクロール操作（C-v など）
(use-package smooth-scroll
  :ensure t
  :config
  (smooth-scroll-mode t)                    ; smooth-scroll モードを有効化
  (setq smooth-scroll/vscroll-step-size 8)  ; 垂直スクロールのステップサイズを設定
  )

;;; Swiper - インクリメンタルサーチ機能の強化
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper))
  :config
  )

;;; Undo-tree - アンドゥの操作のツリー表示と管理
;; C-/ でundo
;; C-x u で樹形図表示
(use-package undo-tree
  :ensure t
  :diminish
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil)  ; アンドゥ履歴の自動保存を無効化

  ;; undo-tree-visualize 関数の動作をカスタマイズ
  (defun undo-tree-split-side-by-side (original-function &rest args)
    "Split undo-tree side-by-side."
    (let ((split-height-threshold nil)   ; 縦分割の閾値
          (split-width-threshold 0))    ; 横分割の閾値
      (apply original-function args)))

  ;; undo-tree-visualize のアドバイスを追加
  (advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side)

  ;; アンドゥツリービジュアライザのキーバインド設定
  (define-key undo-tree-visualizer-mode-map (kbd "RET") 'undo-tree-visualizer-quit)  ; RET で終了
  (define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-quit)  ; C-g で終了
  )

;;; Undohist - アンドゥ履歴のファイル保存
(use-package undohist
  :ensure t
  :diminish  ; モードラインの表示を隠す
  :config
  (setq undohist-directory (my-set-history "undohist")) ; アンドゥ履歴の保存場所 (@00-auto-file-place.el)
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
  (setq amx-save-file (my-set-history "amx-items")) ; Amx の履歴ファイルの保存場所 (@00-auto-file-place.el)
  )

;;; S - 文字列操作のための追加機能
(use-package s
  :ensure t
  )

;;; Yasnippet - コードスニペットの管理と挿入
(use-package yasnippet
  :ensure t
  :diminish
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/my-data/snippets"
                           "~/.emacs.d/my-data/snippets/snippets" ;; シンボリックリンク用
                           "~/.emacs.d/loads/elisp/yasnippet-snippets/snippets"))
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
