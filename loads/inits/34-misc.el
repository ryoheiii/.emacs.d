;;; 34-misc.el --- ユーティリティの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; その他の便利ツール・ユーティリティパッケージの設定

;;; Code:

;;; Which-key - 使用可能なキーバインドの表示
(use-package which-key
  :straight nil
  :preface
  (defun my/which-key-group-bindings (format-function bindings &optional full-key)
    "BINDINGS の同一コマンドのキーをまとめ、説明を一度だけ表示する。
FORMAT-FUNCTION と FULL-KEY による標準の翻訳・装飾・非表示を維持する。"
    (let ((groups (make-hash-table :test #'eq))
          rows)
      (dolist (binding bindings)
        ;; cdr は日本語への置換前に which-key がキーマップから取得した割り当て先。
        ;; prefix や lambda など、コマンドを特定できない表示はまとめない。
        (let* ((command (intern-soft (cdr binding)))
               (row (car (funcall format-function (list binding) full-key)))
               (group (and (commandp command) (not (keymapp command))
                           (gethash command groups))))
          (when row
            (if group
                (setcar group (concat (car group) "/" (car row)))
              (push row rows)
              (when (and (commandp command) (not (keymapp command)))
                (puthash command row groups))))))
      (nreverse rows)))
  :custom
  (which-key-idle-delay 0.5)             ; キーの続きに迷ったら早めに案内
  (which-key-idle-secondary-delay 0.1)
  (which-key-max-description-length 40) ; 説明の最大長
  (which-key-use-C-h-commands nil)      ; DEL に変換される C-h をページ操作として案内しない
  :hook
  (after-init . which-key-mode)          ; Emacs 起動後に which-key モードを有効化
  :config
  (advice-add 'which-key--format-and-replace :around #'my/which-key-group-bindings)
  ;; コマンド名に対する表示の置換なので、モード側がキーを上書きしても誤案内しない。
  (dolist (entry (append my/key-guide-commands my/key-guide--cpp-commands
                        my/key-guide--extra-descriptions))
    (add-to-list 'which-key-replacement-alist
                 (cons (cons nil (concat "\\`" (regexp-quote (symbol-name (cdr entry)))
                                         "\\'"))
                       (cons nil (car entry)))))
  (which-key-add-key-based-replacements "C-c h" "操作ヘルプ"))

;;; Free-keys - 未使用のキーバインドを表示
(use-package free-keys
  :straight t
  :defer t
  :commands (free-keys))

;;; undo-fu-session - undo 情報を Emacs 終了後も保持
(use-package undo-fu-session
  :straight t
  :defer t
  :custom
  (undo-fu-session-directory (my-set-history "undo-fu-session/"))
  ;; undo 永続化はデータ保全に関わるため、アイドル遅延ではなく起動完了直後に有効化。
  ;; 保存先はパッケージ自身が所有者のみ読める権限 (0700) で作成する
  :hook (emacs-startup . undo-fu-session-global-mode))

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
  (vundo-window-max-height 8)  ; vundo ウィンドウの高さを最大 8 行にする
  (vundo-roll-back-on-quit t))  ; `q` で抜けた時に元の位置に戻る

;;; Stopwatch - シンプルなストップウォッチ
(use-package stopwatch
  :straight (stopwatch
             :type git
             :host github
             :repo "blue0513/stopwatch") ; https://github.com/blue0513/stopwatch
  :defer t
  :commands (stopwatch-start stopwatch-stop stopwatch-restart stopwatch-pause))

(provide '34-misc)
;;; 34-misc.el ends here
