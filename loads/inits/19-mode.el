(use-package region-bindings-mode
  :ensure t
  :config
  (region-bindings-mode-enable)
  )

;;; 関数折りたたみ
(add-hook 'c++-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'c-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'lisp-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(define-key global-map (kbd "C-\\") 'hs-toggle-hiding)
(define-key global-map (kbd "<f5>") 'hs-toggle-hiding)

;;; 関連付け
(setq auto-mode-alist
      (append '(
                ("\\.go$"   . go-mode)
                ("\\.C$"    . c-mode)
                ("\\.cc$"   . cc-mode)
                ("\\.nut$"  . c++-mode)
                ("\\.cpp$"  . c++-mode)
                ("\\.hh$"   . c++-mode)
                ("\\.c$"    . c-mode)
                ("\\.h$"    . c++-mode)
                ("\\.hpp$"  . c++-mode)
                ("\\.log$"  . c-mode)    ;; config.log から find-file を使用したいため
                ("\\.cfg$"  . c-mode)    ;; .cfg からタグジャンプしたいため
                ("\\.tmp$"  . text-mode) ;; コミットログ時にスニペットを展開したいため
                ("\\.txt$"  . text-mode)
                ("\\.java$" . java-mode)
                ("\\.el$"   . emacs-lisp-mode)
                ("\\.py$"   . python-mode)
                )auto-mode-alist)
      )

;;; cc-mode
;; c-modeやc++-modeなどcc-modeベースのモード共通の設定
(use-package cc-mode
  :ensure t
  :config
  (add-hook
   'c-mode-common-hook
   (lambda ()
     ;; コンパイル
     (local-set-key (kbd "C-c c") 'compile)

     ;; 自動改行（auto-new-line）と連続する空白の一括削除（hungry-delete）を有効にする
     (c-toggle-auto-hungry-state 1)
     ;; (c-toggle-hungry-state 1)

     ;; 他のエディタなどがファイルを書き換えたらすぐにそれを反映する
     ;; auto-revert-modeを有効にする
     (auto-revert-mode)

     ;; インデント幅を4にする
     (setq indent-tabs-mode nil c-basic-offset 4))
   )
  )

;; c++-modeだけの設定
(add-hook
 'c++-mode-hook
 (lambda ()
   ;; nothing
   ))

;;; text-mode
(add-hook
 'text-mode-hook
 (lambda ()
   ;; ;; 自動で長過ぎる行を分割する
   ;; (auto-fill-mode 1)

   ;; スペースでインデントをする
   (setq indent-tabs-mode nil)

   ;; インデント幅を2にする: 働いていない
   (setq tab-width 2)
   (setq c-basic-offset 2)

   (subword-mode 1))
 )

;;; txt-mode
(add-hook
 'txt-mode-common-hook
 (lambda ()
   (setq c-indent-level 2)

   ;; スペースでインデントをする
   (setq indent-tabs-mode nil)

   ;; インデント幅を2にする
   (setq tab-width 2)
   (setq c-basic-offset 2)

   ;; 自動改行（auto-new-line）と
   ;; 連続する空白の一括削除（hungry-delete）を
   ;; 有効にする
   (c-toggle-auto-hungry-state 1)
   (c-toggle-hungry-state 1)
   (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                      64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

   (subword-mode 1))
 )

;;; emacs-lisp-mode
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   ;; スペースでインデントをする
   (setq indent-tabs-mode nil))
 )
