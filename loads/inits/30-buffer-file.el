;;; 30-buffer-file.el --- バッファとファイル管理の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; バッファ管理、ファイル履歴、スクロール関連パッケージの設定

;;; Code:

;;;;; [Group] Buffer-and-File-management - バッファとファイル管理関連 ;;;;;
;;; xclip - クリップボードとの共有
(use-package xclip
  :if (and (not (display-graphic-p)) ; GUIではない
           (getenv "DISPLAY")        ; X11 の DISPLAY 変数がある
           (executable-find "xclip")) ; `xclip` がシステムにインストールされている
  :straight t
  :defer t
  :init
  ;; xclip-mode は autoload 済み → タイマーでパッケージロード + :config 実行
  (run-with-idle-timer 0.5 nil #'xclip-mode 1)
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
  :after recentf
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

(provide '30-buffer-file)
;;; 30-buffer-file.el ends here
