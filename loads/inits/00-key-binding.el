;;; キーバインド設定
;; C-h でバックスペース
(keyboard-translate ?\C-h ?\C-?)

;; 基本的なキーバインド
(global-set-key (kbd "C-c a") 'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(global-set-key (kbd "C-c d")   'delete-indentation)
(define-key global-map (kbd "M-?") 'help-for-help)                 ; ヘルプ
(define-key global-map (kbd "C-c C-i") 'hippie-expand)             ; 補完
(define-key global-map (kbd "C-c ;") 'comment-or-uncomment-region) ; コメントアウト
(define-key global-map (kbd "C-.") 'goto-line)                     ; 指定行へ移動
(define-key global-map (kbd "M-f") 'forward-symbol)
(global-set-key [f7] 'toggle-truncate-lines)                       ; 折り返しの有効無効切替
(global-set-key (kbd "C-c 0") 'my/copy-file-path)

;;; ウィンドウ操作に関するキーバインド
(global-set-key "\C-c\C-r" 'window-resizer)                     ; ウィンドウリサイズ
(define-key global-map (kbd "M-p") 'other-window-or-split)      ; 次のウィンドウへ移動
;; (define-key global-map (kbd "C-t") 'next-multiframe-window)     ; 次のウィンドウへ移動
;; (define-key global-map (kbd "C-q") 'previous-multiframe-window) ; 前のウィンドウへ移動
