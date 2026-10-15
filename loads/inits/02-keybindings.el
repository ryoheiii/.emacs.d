;;; 02-keybindings.el --- キーバインド設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs のキーバインド設定

;;; Code:

;;;;;; [Group] Basic Keybindings - 基本キーバインド ;;;;;;
;; C-h をバックスペースへ変換(key-translation-map は全端末・全フレームで有効)
(keymap-set key-translation-map "C-h" "DEL")

;; 汎用的な操作
(defun my/input-method-off ()
  "IME を OFF にする。"
  (interactive)
  (when current-input-method
    (toggle-input-method)))
(defun my/input-method-on ()
  "IME を ON にする。"
  (interactive)
  (unless current-input-method
    (toggle-input-method)))

(keymap-global-set "C-c a" 'align)                               ; 文字列の揃え
(keymap-global-set "C-c M-a" 'align-regexp)                      ; 正規表現での整列
(keymap-global-set "C-c d" 'delete-indentation)                  ; インデント削除
(keymap-global-set "M-?" 'help-for-help)                         ; ヘルプ
(keymap-global-set "C-c C-i" 'hippie-expand)                     ; 補完
(keymap-global-set "C-c ;" 'comment-or-uncomment-region)         ; コメントアウト
(keymap-global-set "M-f" 'forward-symbol)                        ; シンボル単位の移動
(keymap-global-set "<f7>" 'toggle-truncate-lines)                ; 折り返しの有効無効切替
(keymap-global-set "C-c 0" 'my/copy-file-name)                   ; ファイル名コピー
(keymap-global-set "<zenkaku-hankaku>" 'toggle-input-method)
(keymap-global-set "<muhenkan>" #'my/input-method-off)    ; IME を OFF
(keymap-global-set "<henkan>" #'my/input-method-on)       ; IME を ON

;;;;;; [Group] Window Management - ウィンドウ操作 ;;;;;;
(keymap-global-set "C-c C-r" #'my/window-resizer)           ; ウィンドウサイズ調整
(keymap-global-set "M-p" #'my/other-window-or-split)        ; 次のウィンドウへ移動

(provide '02-keybindings)
;;; 02-keybindings.el ends here
