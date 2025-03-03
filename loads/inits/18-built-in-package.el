;;; 19-built-in-package.el --- 組み込みパッケージの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs 組み込みパッケージの設定

;;; Code:

;;;;;; [Group] Diff & Comparison - 差分・比較 ;;;;;;
(use-package ediff
  :straight nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain) ;; コントロールバッファを同一フレームに表示
  (ediff-split-window-function 'split-window-horizontally) ;; diff のバッファを左右に配置
  )

;;;;;; [Group] Completion - 補完 ;;;;;;
;;; Icomplete - 補完可能なものを随時表示
(use-package icomplete
  :straight nil
  :hook (after-init . icomplete-mode)
  )

;;;;;; [Group] Visual Enhancements - 視覚的な補助 ;;;;;;
;;; Paren - 括弧の対応関係を視覚化する設定。カーソル位置の括弧ペアを強調表示
(use-package paren
  :straight nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-style 'mixed) ;; ウィンドウ内に収まらないときだけ括弧内も光らせる
  )

;;; Whitespace - 空白文字の視覚化。スペース、タブ、行末の空白などを明確に表示
(use-package whitespace
  :straight nil
  :hook (after-init . global-whitespace-mode)
  :custom
  (whitespace-style '(face
                      trailing   ; 行末
                      tabs
                      spaces
                      empty      ; 先頭/末尾の空行
                      space-mark ; 表示のマッピング
                      tab-mark
                      newline))
  (whitespace-display-mappings '((space-mark ?\x3000 [?\u25a1]) (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (whitespace-space-regexp "\\(\u3000+\\)") ; スペースは全角のみを可視化
  (whitespace-trailing-regexp "\\([ \u00A0]+\\)$")
  :config
  (set-face-attribute 'whitespace-trailing nil :background "Black" :foreground "DeepPink" :underline t)
  (set-face-attribute 'whitespace-tab nil :background "Black" :foreground "DarkMagenta" :underline t)
  (set-face-attribute 'whitespace-space nil :background "Black" :foreground "GreenYellow" :weight 'bold)
  (set-face-attribute 'whitespace-empty nil :background "Black")
  )

;;; Display-fill-column-indicator - テキストの折り返し位置を視覚的に示す
(use-package display-fill-column-indicator
  :straight nil
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (markdown-mode . display-fill-column-indicator-mode))
  :custom
  (display-fill-column-indicator-column 120)
  (display-fill-column-indicator-character ?|)
  )

;;; 表示設定
(use-package display-time
  :straight nil
  :if window-system
  :hook (after-init . display-time-mode)
  :custom
  (display-time-day-and-date t)
  (display-time-string-forms '((format "%s/%s (%s) %s:%s"
                                       month day dayname 24-hours minutes)))
  )

;;;;;; [Group] Search - 検索 ;;;;;;
;;; Grep - ファイル内検索機能の設定。特定のパターンに基づいてファイルを検索
(use-package grep
  :straight nil
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

;;;;;; [Group] Editing - 編集補助 ;;;;;;
;; Elec-pair - 括弧の自動補完設定。入力中の括弧を自動的にペアで補完
(use-package elec-pair
  :straight nil
  :hook (after-init . electric-pair-mode)
  )

;;; Savehist - 履歴の保存設定。検索履歴やコマンド履歴をファイルに保存
(use-package savehist
  :straight nil
  :hook (after-init . savehist-mode)
  :custom
  (history-length 3000)
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-file (my-set-history "savehist")) ;; my-set-history @early-init.el
  )

;;; Saveplace - カーソル位置の保存。ファイルを再度開いた時に前回のカーソル位置を保持
(use-package saveplace
  :straight nil
  :hook (after-init . save-place-mode)
  :custom
  (save-place-file (my-set-history "places")) ;; my-set-history @early-init.el
  )

;;; 他プロセスの編集をバッファに反映
(use-package autorevert
  :straight nil
  :hook (after-init . global-auto-revert-mode)
  :custom (magit-auto-revert-mode t)
  )

;;; Auto-save-visited - 一定時間経過しても操作がない場合、バッファを自動保存
(use-package files
  :straight nil
  :custom (auto-save-visited-interval 30)
  :config
  (auto-save-visited-mode +1)
  )

;;;;;; [Group] Window & Buffer Management - ウィンドウ・バッファ管理 ;;;;;;
;;; Windmove - ウィンドウ間の移動設定。Shift + 矢印キーでウィンドウ間を移動
(use-package windmove
  :straight nil
  :config
  (windmove-default-keybindings) ;; Shift + 矢印キーでウィンドウ間を移動
  )

;;; Uniquify - バッファ名のユニーク化。同名ファイルを開いた際にディレクトリ名で区別
(use-package uniquify
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*")
  )

;;; Tab-bar - Emacsのタブ機能をカスタマイズ
(use-package tab-bar
  :straight nil
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-tab-name-function 'tab-bar-tab-name-all)
  :config
  ;; C-z をプレフィックスキーとして定義
  (define-prefix-command 'tab-bar-prefix-map)      ; 新しいプレフィックスコマンドを定義
  (global-set-key (kbd "C-z") 'tab-bar-prefix-map) ; C-z をプレフィックスキーとして設定
  ;; 各種キーバインド設定
  (define-key tab-bar-prefix-map (kbd "n") 'tab-next)
  (define-key tab-bar-prefix-map (kbd "C-n") 'tab-next)
  (define-key tab-bar-prefix-map (kbd "p") 'tab-previous)
  (define-key tab-bar-prefix-map (kbd "C-p") 'tab-previous)
  (define-key tab-bar-prefix-map (kbd "f") 'tab-new)
  (define-key tab-bar-prefix-map (kbd "C-f") 'tab-new)
  (define-key tab-bar-prefix-map (kbd "k") 'tab-close)
  (define-key tab-bar-prefix-map (kbd "C-k") 'tab-close)
  ;; 特定のタブを選択するためのキーバインド設定
  (dotimes (i 9)
    (define-key tab-bar-prefix-map (kbd (number-to-string (1+ i)))
                `(lambda () (interactive) (tab-bar-select-tab ,(1+ i)))))
  )

;;;;;; [Group] Performance Optimization - パフォーマンス最適化 ;;;;;;
;;; So-long - 長い行を含むファイルを最適化
(use-package so-long
  :straight nil
  :hook (after-init . global-so-long-mode)
  )

;;; repeat - キーのリピート (multiple-cursors などで利用)
(use-package repeat
  :straight nil
  :hook (after-init . repeat-mode)
  )

(provide '18-built-in-package)
;;; 18-built-in-package.el ends here
