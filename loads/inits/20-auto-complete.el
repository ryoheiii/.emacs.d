;;; 自動補完機能-auto-complete
;; 130502
(use-package auto-complete
  :ensure t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/my-data/ac-dict") ;; ディレクトリ指定
(ac-config-default)

(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'yatex-mode)

;; トリガーキーを押さないと補完メニューを出さないようにする
; (setq ac-auto-start nil)
(ac-set-trigger-key "TAB")

;; 補完機能をC-n C-pで移動
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;;; コメントや文字列リテラルでも補完を行う
;; 130723
;; (setq ac-disable-faces nil)

(setq ac-use-fuzzy t)          ;; 曖昧マッチ

;; yasnippetのbindingを指定するとエラーが出るので回避する方法。
(setf (symbol-function 'yas-active-keys)
      (lambda ()
        (remove-duplicates (mapcan #'yas--table-all-keys (yas--get-snippet-tables)))))
