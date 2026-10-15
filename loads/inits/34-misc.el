;;; 34-misc.el --- ユーティリティの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; その他の便利ツール・ユーティリティパッケージの設定

;;; Code:

;;;;; [Group] Misc-utilities - その他のユーティリティ ;;;;;
;;; Which-key - 使用可能なキーバインドの表示
(use-package which-key
  :straight nil
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

;;; undo-fu-session - undo 情報を Emacs 終了後も保持
(use-package undo-fu-session
  :straight t
  :defer t
  :custom
  (undo-fu-session-directory (my-set-history "undo-fu-session/"))
  ;; undo 永続化はデータ保全に関わるため、アイドル遅延ではなく起動完了直後に有効化。
  ;; 保存先はパッケージ自身が所有者のみ読める権限 (0700) で作成する
  :hook (emacs-startup . undo-fu-session-global-mode)
  )

;;; Vundo - アンドゥの操作をツリー表示で管理
(use-package vundo
  :straight t
  :bind (("C-x u" . vundo)
         :map vundo-mode-map
         ("C-f" . vundo-forward)    ; 次の状態へ (→)
         ("C-b" . vundo-backward)   ; 前の状態へ (←)
         ("C-n" . vundo-next)       ; 下の分岐へ (↓)
         ("C-p" . vundo-previous))  ; 上の分岐へ (↑)
  :custom
  (vundo-compact-display nil)  ; 画面を広めに使う
  (vundo-window-max-height 8)  ; vundo ウィンドウの高さを大きくする
  (vundo-roll-back-on-quit t)  ; `q` で抜けた時に元の位置に戻る
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
