;;; 34-misc.el --- ユーティリティの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; その他の便利ツール・ユーティリティパッケージの設定

;;; Code:

;;;;; [Group] Misc-utilities - その他のユーティリティ ;;;;;
;;; Paradox - パッケージ管理 UI の強化
(use-package paradox
  :straight t
  :defer t
  :commands (paradox-list-packages paradox-upgrade-packages)
  :custom
  (paradox-github-token t)
  :config
  (paradox-enable)
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
  :defer t
  :commands (free-keys)
  )

;;; Amx - M-x コマンドの履歴強化
(use-package amx
  :straight t
  :defer t
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
  :defer t
  :custom
  (undo-fu-session-directory (my-set-history "undo-fu-session/"))
  :init
  ;; undo 永続化はデータ保全に関わるため、アイドル遅延ではなく起動完了直後に有効化
  (add-hook 'emacs-startup-hook (lambda () (undo-fu-session-global-mode 1)))
  :config
  (unless (file-exists-p undo-fu-session-directory)
    (make-directory undo-fu-session-directory t))
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

;;; Stopwatch - シンプルなストップウォッチ
(use-package stopwatch
  :straight (stopwatch
             :type git
             :host github
             :repo "blue0513/stopwatch") ; https://github.com/blue0513/stopwatch
  :defer t
  :commands (stopwatch-start stopwatch-stop stopwatch-restart stopwatch-pause)
  )

(provide '34-misc)
;;; 34-misc.el ends here
