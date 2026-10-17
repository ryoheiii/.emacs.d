;;; 34-misc.el --- ユーティリティの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; その他の便利ツール・ユーティリティパッケージの設定

;;; Code:

;;; Which-key - 使用可能なキーバインドの表示
(use-package which-key
  :straight nil
  :preface
  (declare-function which-key--popup-max-dimensions "which-key")
  (defvar my/which-key--keymap nil
    "which-key の候補を取得しているキーマップ。")
  (defun my/which-key-with-keymap (function &optional prefix keymap filter recursive)
    "FUNCTION による候補取得中、実際の割り当てを参照できるようにする。"
    (let ((my/which-key--keymap
           (or keymap
               (make-composed-keymap
                (append (current-active-maps t)
                        (mapcar #'symbol-value which-key-extra-keymaps))))))
      (funcall function prefix keymap filter recursive)))
  (defun my/which-key-group-bindings (format-function bindings &optional full-key)
    "BINDINGS の同一コマンドのキーを、端末幅に収まる単位でまとめる。
FORMAT-FUNCTION と FULL-KEY による標準の翻訳・装飾・非表示を維持する。"
    (let ((groups (make-hash-table :test #'eq))
          ;; 説明欄を確保し、多数の別名キーも全て読める幅で小分けにする。
          (key-width-limit (max 1 (/ (cdr (which-key--popup-max-dimensions)) 4)))
          rows)
      (dolist (binding bindings)
        ;; 表示用の文字列から型は復元できないため、元のキーで実体を取得する。
        ;; シンボルコマンドだけをまとめ、マクロ・prefix・無名関数は維持する。
        (let* ((command (and my/which-key--keymap
                             (lookup-key my/which-key--keymap (kbd (car binding)))))
               (command (and (symbolp command) (commandp command)
                             (not (keymapp command)) command))
               (command (and command
                             (or (and which-key-compute-remaps
                                      (command-remapping command))
                                 command)))
               (row (car (funcall format-function (list binding) full-key)))
               (group (and command (gethash command groups)))
               (keys (and group row (concat (car group) "/" (car row)))))
          (when row
            (if (and keys (<= (string-width keys) key-width-limit))
                (setcar group keys)
              (push row rows)
              (when command
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
  (advice-add 'which-key--get-bindings :around #'my/which-key-with-keymap)
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
