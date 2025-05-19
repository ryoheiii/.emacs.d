;;; 02-keybindings.el --- キーバインド設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs のキーバインド設定

;;; Code:

;;;;;; [Group] Basic Keybindings - 基本キーバインド ;;;;;;
;; C-h をバックスペースに変更
(keyboard-translate ?\C-h ?\C-?)

;; 汎用的な操作
(global-set-key (kbd "C-c a") 'align)                            ; 文字列の揃え
(global-set-key (kbd "C-c M-a") 'align-regexp)                   ; 正規表現での整列
(global-set-key (kbd "C-c d") 'delete-indentation)               ; インデント削除
(global-set-key (kbd "M-?") 'help-for-help)                      ; ヘルプ
(global-set-key (kbd "C-c C-i") 'hippie-expand)                  ; 補完
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)      ; コメントアウト
(global-set-key (kbd "C-.") 'goto-line)                          ; 指定行へ移動
(global-set-key (kbd "M-f") 'forward-symbol)                     ; シンボル単位の移動
(global-set-key [f7] 'toggle-truncate-lines)                     ; 折り返しの有効無効切替
(global-set-key (kbd "C-c 0") 'my/copy-file-name)                ; ファイル名コピー
(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
(global-set-key (kbd "<muhenkan>") (lambda ()
                                     (interactive)
                                     (when current-input-method
                                       (toggle-input-method))))  ; IME を OFF
(global-set-key (kbd "<henkan>") (lambda ()
                                   (interactive)
                                   (unless current-input-method
                                     (toggle-input-method))))    ; IME を ON

;;;;;; [Group] Window Management - ウィンドウ操作 ;;;;;;
(global-set-key (kbd "C-c C-r") 'window-resizer)            ; ウィンドウサイズ調整
(global-set-key (kbd "M-p") 'other-window-or-split)         ; 次のウィンドウへ移動
;; (global-set-key (kbd "C-t") 'next-multiframe-window)     ; 次のフレームのウィンドウへ移動
;; (global-set-key (kbd "C-q") 'previous-multiframe-window) ; 前のフレームのウィンドウへ移動

(provide '02-keybindings.el)
;;; 02-keybindings.el ends here
