;;; 98-backup.el --- 置き換えが済み次第削除 -*- lexical-binding: t; -*-
;;; Commentary:
;; 置き換え元パッケージの設定

;;; Code:

;;;;; ■ helm-gtags を置きかえ
;;; Xref - ソースコードのタグナビゲーション
;; (use-package xref
;;   :straight nil
;;   :init
;;   ;; `C-t` をプレフィックスキーに設定
;;   (define-prefix-command 'my-gtags-prefix)
;;   (global-set-key (kbd "C-t") 'my-gtags-prefix)
;;   :hook ((c-mode . my-setup-gtags)
;;          (c++-mode . my-setup-gtags))
;;   :bind (:map my-gtags-prefix
;;               (
;;                ("d"   . xref-find-definitions)  ; 関数の定義場所を検索
;;                ("C-d" . xref-find-definitions)
;;                ("u"   . xref-find-references)   ; 関数の使用箇所を検索
;;                ("C-u" . xref-find-references)
;;                ("v"   . consult-imenu)          ; シンボル検索
;;                ("C-v" . consult-imenu)
;;                ("f"   . project-find-file)      ; ファイル検索
;;                ("C-f" . project-find-file)
;;                ("p"   . xref-pop-marker-stack)  ; 戻る
;;                ("C-p" . xref-pop-marker-stack)
;;                ("n"   . xref-find-references)   ; 進む
;;                ("C-n" . xref-find-references)
;;                ))
;;   :config
;;   ;;;;; `global -uv` による GTAGS の自動更新
;;   (defun update-gtags ()
;;     "GTAGS データベースを自動更新."
;;     (interactive)
;;     (when (and (buffer-file-name) (executable-find "global"))
;;       (start-process "gtags-update" nil "global" "-uv")))

;;   (defun my-setup-gtags ()
;;     "C/C++ モード時に GTAGS の自動更新を設定."
;;     (add-hook 'after-save-hook #'update-gtags nil 'local))
;;   )





;;;;; ■ バックアップ
;;; company - 自動補完の基本設定
;; (use-package company
;;   :straight t
;;   :hook (after-init . global-company-mode)
;;   :bind (("C-M-i" . company-complete)
;;          :map company-mode-map
;;          ("TAB" . indent-for-tab-command)
;;          :map company-active-map
;;          ("M-n" . nil)                        ; M-n で次の候補への移動をキャンセル
;;          ("M-p" . nil)                        ; M-p で前の候補への移動をキャンセル
;;          ("C-n" . company-select-next)        ; 次の補完候補を選択
;;          ("C-p" . company-select-previous)    ; 前の補完候補を選択
;;          ("C-s" . company-filter-candidates)  ; C-s で絞り込む
;;          ("TAB" . company-complete-selection)
;;          :map company-search-map
;;          ("C-n" . company-select-next)
;;          ("C-p" . company-select-previous))
;;   :custom
;;   (company-selection-wrap-around t)                            ; 最後の候補で次の候補にループ
;;   (company-transformers '(company-sort-by-occurrence
;;                           company-sort-by-backend-importance)) ; 頻度順ソート
;;   (company-idle-delay 0)                                       ; デフォルトは 0.5
;;   (company-show-numbers t)
;;   (company-tooltip-limit 10)
;;   (company-minimum-prefix-length 2)                            ; デフォルトは 4
;;   (company-tooltip-align-annotations t)
;;   (company-tooltip-flip-when-above t)
;;   (company-dabbrev-around t)
;;   (completion-ignore-case t)
;;   (company-dabbrev-downcase nil)
;;   (company-eclim-auto-save nil)
;;   (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
;;                        company-echo-metadata-frontend))
;;   :config
;;   ;; ツールチップの色設定
;;   ;; 基本設定
;;   (set-face-attribute 'company-tooltip nil
;;                       :foreground "white" :background "midnight blue")
;;   (set-face-attribute 'company-tooltip-common nil
;;                       :foreground "white" :background "midnight blue")
;;   ;; 選択項目の設定
;;   (set-face-attribute 'company-tooltip-common-selection nil
;;                       :foreground "white" :background "dark slate blue")
;;   (set-face-attribute 'company-tooltip-selection nil
;;                       :foreground "white" :background "dark slate blue")
;;   ;; プレビューとスクロールバーの設定
;;   (set-face-attribute 'company-preview-common nil
;;                       :background "dark slate blue" :foreground "white" :underline t)
;;   (set-face-attribute 'company-scrollbar-fg nil
;;                       :background "dark gray")
;;   (set-face-attribute 'company-scrollbar-bg nil
;;                       :background "dim gray")

;;   ;; company-backends の設定
;;   (setq company-backends
;;         '((company-capf :with company-same-mode-buffers company-yasnippet)
;;           (company-dabbrev-code :with company-same-mode-buffers company-yasnippet)
;;           (company-irony-c-headers company-irony)   ; Irony と Irony-C-Headers の組み合わせ
;;           company-keywords
;;           company-files
;;           company-dabbrev))
;;   (add-to-list 'company-backends 'company-files)    ; ファイル名補完
;;   (add-to-list 'company-backends 'company-keywords) ; 言語のキーワード補完
;;   )

;; ;;; Company-statistics - よく使う補完候補を優先表示
;; (use-package company-statistics
;;   :straight t
;;   :after company
;;   :custom
;;   (company-statistics-file (my-set-history "company-statistics-cache.el")) ; 履歴保存場所
;;   :config
;;   (company-statistics-mode)
;;   )

;; ;;; company-same-mode-buffers - 同じモード内でのキーワード補完を強化
;; (use-package company-same-mode-buffers
;;   :straight '(company-same-mode-buffers
;;               :type git
;;               :host github
;;               :repo "zk-phi/company-same-mode-buffers")
;;   :after company
;;   :config
;;   (company-same-mode-buffers-initialize)
;;   )

;; ;;; Company-irony - C/C++ 用の補完
;; (use-package company-irony
;;   :straight t
;;   :after (company irony)
;;   )

;; ;;; Company-irony-c-headers - C ヘッダファイル用の補完（Company バックエンド）
;; (use-package company-irony-c-headers
;;   :straight t
;;   :after (company irony)
;;   )

;; ;;; company-dwim - 候補の絞り込みをスマート化（補完フロントエンド）
;; (use-package company-dwim
;;   :straight '(company-dwim
;;               :type git
;;               :host github
;;               :repo "zk-phi/company-dwim")
;;   :after company
;;   :bind (:map company-active-map
;;               ("TAB" . company-dwim))
;;   :config
;;   (add-to-list 'company-frontends 'company-dwim-frontend)
;;   )

;; ;;; company-anywhere - どこでも補完を可能にする
;; (use-package company-anywhere
;;   :straight '(company-anywhere
;;               :type git
;;               :host github
;;               :repo "zk-phi/company-anywhere")
;;   :after company
;;   )

;; ;;; Company-box - Company の補完候補をパネル表示 (GUI 限定)
;; (use-package company-box
;;   :straight t
;;   :if (display-graphic-p)
;;   :hook (company-mode . company-box-mode)
;;   )

;; ;;; company-posframe - GUI での補完候補パネル表示
;; (use-package company-posframe
;;   :straight t
;;   :if (display-graphic-p)
;;   :after company
;;   :config
;;   (company-posframe-mode 1)
;;   )

;; ;;; Helm - 効率的なバッファやコマンドの検索
;; (use-package helm
;;   :straight t
;;   :after migemo
;;   :init
;;   (helm-mode 1)
;;   (helm-migemo-mode 1)
;;   :bind (("M-x"     . helm-M-x)
;;          ("C-x C-f" . helm-find-files)
;;          ("C-x C-r" . helm-recentf)
;;          ("C-x C-y" . helm-show-kill-ring)
;;          ("C-x g"   . helm-do-grep-ag)
;;          ;; ("C-x b"   . helm-buffers-list)
;;          ("C-x b"   . helm-mini)
;;          ("C-x i"   . helm-imenu)
;;          :map helm-map
;;          ("C-h" . delete-backward-char)
;;          :map helm-find-files-map
;;          ("C-h" . delete-backward-char)
;;          ("TAB" . helm-execute-persistent-action)
;;          :map helm-read-file-map
;;          ("TAB" . helm-execute-persistent-action))
;;   :custom
;;   (helm-display-function #'display-buffer)       ; 'display-buffer' を使用
;;   (helm-echo-input-in-header-line t)             ; 入力をヘッダーラインに表示
;;   (helm-input-idle-delay 0.2)                    ; 入力から画面更新までの遅延時間 (デフォルトは 0.01 秒)
;;   (helm-split-window-inside-p t)                 ; Helm のウィンドウを現在のウィンドウ内に分割して表示
;;   (helm-move-to-line-cycle-in-source t)          ; バッファ内で候補の最後に到達したら先頭に戻る
;;   (helm-ff-file-name-history-use-recentf t)      ; 'recentf' をファイル名の履歴として使用
;;   (helm-ff-search-library-in-sexp t)             ; SEXP 内でライブラリ検索
;;   (helm-ff-fuzzy-matching nil)                   ; ファジーマッチを無効化
;;   (helm-buffer-details-flag nil)                 ; バッファの詳細を非表示
;;   (helm-delete-minibuffer-contents-from-point t) ; ミニバッファの内容を削除
;;   :config
;;   ;; Emacsのコマンドと履歴の Helm ソース定義
;;   (defvar helm-source-emacs-commands
;;     (helm-build-sync-source "Emacs commands"
;;       :candidates (lambda ()
;;                     (let ((cmds))
;;                       (mapatoms (lambda (elt) (when (commandp elt) (push elt cmds))))
;;                       cmds))
;;       :coerce #'intern-soft
;;       :action #'command-execute)
;;     "A Helm source for Emacs commands.")

;;   (defvar helm-source-emacs-commands-history
;;     (helm-build-sync-source "Emacs commands history"
;;       :candidates (lambda ()
;;                     (let ((cmds))
;;                       (dolist (elem extended-command-history)
;;                         (push (intern elem) cmds))
;;                       cmds))
;;       :coerce #'intern-soft
;;       :action #'command-execute)
;;     "Emacs commands history.")

;;   ;; 検索パターンの変換
;;   (defun helm-buffers-list-pattern-transformer (pattern)
;;     "Helm のバッファ検索時のパターン変換."
;;     (if (equal pattern "")
;;         pattern
;;       (let* ((first-char (substring pattern 0 1))
;;              (pattern (cond ((equal first-char "*") (concat " " pattern))
;;                             ((equal first-char "=") (concat "*" (substring pattern 1)))
;;                             (t pattern))))
;;         (setq pattern (replace-regexp-in-string "\\." "\\\\." pattern))
;;         (setq pattern (replace-regexp-in-string "\\*" "\\\\*" pattern))
;;         pattern)))

;;   ;; 'kill-line' のエミュレーション
;;   (advice-add 'helm-delete-minibuffer-contents
;;               :before
;;               (lambda ()
;;                 "Emulate `kill-line` in Helm minibuffer."
;;                 (kill-new (buffer-substring (point) (field-end)))))

;;   ;; 'helm-ff-kill-or-find-buffer-fname' をファイルが存在する場合のみ実行
;;   (advice-add 'helm-ff-kill-or-find-buffer-fname
;;               :around
;;               (lambda (orig-fun &rest args)
;;                 "Execute command only if CANDIDATE exists."
;;                 (when (file-exists-p (car args))
;;                   (apply orig-fun args))))

;;   ;; 'helm-ff--transform-pattern-for-completion' のカスタマイズ
;;   (advice-add 'helm-ff--transform-pattern-for-completion
;;               :around
;;               (lambda (orig-fun pattern)
;;                 "Transform the pattern to reflect intention."
;;                 (let* ((input-pattern (file-name-nondirectory pattern))
;;                        (dirname (file-name-directory pattern)))
;;                   (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
;;                   (concat dirname (if (string-match "^\\^" input-pattern)
;;                                       ;; '^' is a pattern for basename
;;                                       ;; and not required because the directory name is prepended
;;                                       (substring input-pattern 1)
;;                                     (concat ".*" input-pattern))))))


;;   ;; helm-migemo fix
;;   ;; http://emacs.rubikitch.com/helm-migemo/
;;   (with-eval-after-load "helm-migemo"
;;     (defun helm-compile-source--candidates-in-buffer (source)
;;       (helm-aif (assoc 'candidates-in-buffer source)
;;           (append source
;;                   `((candidates
;;                      . ,(or (cdr it)
;;                             (lambda ()
;;                               (helm-candidates-in-buffer (helm-get-current-source)))))
;;                     (volatile) (match identity)))
;;         source))
;;     (defalias 'helm-mp-3-get-patterns 'helm-mm-3-get-patterns)
;;     (defalias 'helm-mp-3-search-base 'helm-mm-3-search-base))
;;   )

;; ;;; Helm-c-yasnippet - Helm 経由のスニペット選択
;; (use-package helm-c-yasnippet
;;   :straight t
;;   :bind (("C-c y" . helm-yas-complete))
;;   :custom
;;   (helm-yas-space-match-any-greedy t) ; Helm のスペースマッチングを有効化
;;   )

;; ;;; Helm-ag - ファイルの内容を高速検索
;; (use-package helm-ag
;;   :straight t
;;   )

;; ;;; Helm-swoop - インクリメンタルサーチ機能の強化
;; (use-package helm-swoop
;;   :straight t
;;   :bind (("C-x s" . helm-swoop))
;;   :custom-face
;;   ;; 検索行のハイライト色を設定（例：明るい青の背景と白のテキスト）
;;   (helm-swoop-target-line-face ((t (:background "#0077ff" :foreground "white"))))
;;   ;; 検索語のハイライト色を設定（例：明るい緑の背景と黒のテキスト）
;;   (helm-swoop-target-word-face ((t (:background "#00ff00" :foreground "black"))))
;;   )

;; ;;; Ivy - 効率的なバッファやファイルの検索 (swiper の強化)
;; (use-package ivy
;;   :straight t
;;   :after ivy-migemo
;;   :init
;;   (ivy-mode 1)
;;   :custom
;;   (ivy-use-virtual-buffers t)
;;   (enable-recursive-minibuffers t)
;;   (ivy-height 30)
;;   (ivy-extra-directories nil)
;;   ;; swiper と連携
;;   ;; https://github.com/ROCKTAKEY/ivy-migemo
;;   (ivy-re-builders-alist '((t . ivy--regex-plus)
;;                            (swiper . ivy-migemo--regex-plus)
;;                            ;; (t . ivy--regex-fuzzy)             ; fuzzy バージョン
;;                            ;; (swiper . ivy-migemo--regex-fuzzy) ; fuzzy バージョン
;;                            ))
;;   )

;; ;;; Ivy-migemo - Ivy で Migemo を利用 (Swiper/Ivy の強化)
;; (use-package ivy-migemo
;;   :straight t
;;   )

;; ;;; Swiper - インクリメンタルサーチ機能の強化
;; (use-package swiper
;;   :straight t
;;   :bind (("C-s" . swiper))
;;   )
