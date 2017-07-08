;;; Code:

;;; キーバインド
;; C-hでバックスペース
;; 2012-03-18
(keyboard-translate ?\C-h ?\C-?)
;; 基本
(global-set-key (kbd "C-c a") 'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(global-set-key (kbd "C-c d")   'delete-indentation)
(define-key global-map (kbd "M-?") 'help-for-help)        ; ヘルプ
;(define-key global-map (kbd "C-z") 'undo)                 ; undo
(define-key global-map (kbd "C-c C-i") 'hippie-expand)    ; 補完
(define-key global-map (kbd "C-c ;") 'comment-or-uncomment-region) ; コメントアウト
(define-key global-map (kbd "M-C-g") 'grep)               ; grep
(define-key global-map (kbd "C-.") 'goto-line)      ; 指定行へ移動

;; flycheck用(特別)
(define-key global-map (kbd "C-c C-n") 'flycheck-next-error) ; 次のエラーへ移動
(define-key global-map (kbd "C-c C-p") 'flycheck-previous-error) ; 前のエラーへ移動

;; ウィンドウ移動
;; 2011-02-17
;; 次のウィンドウへ移動
;;(define-key global-map (kbd "C-t") 'next-multiframe-window)
(define-key global-map (kbd "M-p") 'other-window-or-split)
;; 前のウィンドウへ移動
(define-key global-map (kbd "C-q") 'previous-multiframe-window)
;; 定義へ移動
;; 2012-04-15
;; C-x F -> 関数定義へ移動
;; C-x K -> キーにバインドされている関数定義へ移動
;; C-x V -> 変数定義へ移動
(find-function-setup-keys)

;; 121207
;; C-xC-b で electric-buffer-list
;; ebuff-menu 時, "x"で Buffer-menu-execute
(define-key ctl-x-map "\C-b"  'electric-buffer-list)
(eval-after-load "ebuff-menu"
  '(progn
     (define-key
       electric-buffer-menu-mode-map
       "x" 'Buffer-menu-execute)))
