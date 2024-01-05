;;; リージョンバインディングモードの設定
(use-package region-bindings-mode
  :ensure t
  :config
  (region-bindings-mode-enable)
  )

;;; 関数折りたたみ（hs-minor-mode）の設定
(defun my/enable-hs-minor-mode ()
  "hs-minor-modeを有効化するためのヘルパー関数。"
  (hs-minor-mode 1))
(add-hook 'c++-mode-hook        'my/enable-hs-minor-mode)
(add-hook 'c-mode-hook          'my/enable-hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'my/enable-hs-minor-mode)
(add-hook 'lisp-mode-hook       'my/enable-hs-minor-mode)

;;; 関数折りたたみのためのキーバインド
(global-set-key (kbd "C-\\") 'hs-toggle-hiding)
(global-set-key (kbd "<f5>") 'hs-toggle-hiding)

;;; ファイル拡張子とメジャーモードの関連付け
(setq auto-mode-alist
      (append
       '(( "\\.go$"   . go-mode)
         ("\\.C$"    . c-mode)
         ("\\.cc$"   . cc-mode)
         ("\\.nut$"  . c++-mode)
         ("\\.cpp$"  . c++-mode)
         ("\\.hh$"   . c++-mode)
         ("\\.c$"    . c-mode)
         ("\\.h$"    . c++-mode)
         ("\\.hpp$"  . c++-mode)
         ("\\.log$"  . c-mode)    ; config.log から find-file を使用したいため
         ("\\.cfg$"  . c-mode)    ; .cfg からタグジャンプしたいため
         ("\\.tmp$"  . text-mode) ; コミットログ時にスニペットを展開したいため
         ("\\.txt$"  . text-mode)
         ("\\.java$" . java-mode)
         ("\\.el$"   . emacs-lisp-mode)
         ("\\.py$"   . python-mode))
       auto-mode-alist)
      )

;;; cc-mode の設定
(use-package cc-mode
  :ensure t
  :hook (c-mode-common . my/cc-mode-setup)
  :config
  (defun my/cc-mode-setup ()
    ;; コンパイル
    (local-set-key (kbd "C-c c") 'compile)
    ;; 自動改行（auto-new-line）と連続する空白の一括削除（hungry-delete）を有効にする
    (c-toggle-auto-hungry-state 1)
    ;; 他のエディタがファイルを書き換えたらすぐに反映する
    (auto-revert-mode)
    ;; インデント幅を4にする
    (setq indent-tabs-mode nil c-basic-offset 4))
  )

;;; c++-mode の設定
(add-hook 'c++-mode-hook
          (lambda ()
            ;; nothing
            ))

;;; text-mode
(add-hook 'text-mode-hook
          (lambda ()
            ;; スペースでのインデントを有効化
            (setq indent-tabs-mode nil)
            ;; インデント幅を 2 に設定
            (setq tab-width 2)
            (setq c-basic-offset 2)
            ;; subwordモードの有効化（キャメルケースの単語をサブワードとして認識）
            (subword-mode 1))
          )

;;; txt-mode
;; 注意: txt-mode-common-hook は標準では存在しないカスタムフックと仮定
(add-hook 'txt-mode-common-hook
          (lambda ()
            ;; インデントレベルとスペースによるインデントの設定
            (setq c-indent-level 2)
            (setq indent-tabs-mode nil) ;; スペースでインデント
            (setq tab-width 2)
            (setq c-basic-offset 2)
            ;; 自動改行（auto-new-line）と連続する空白の一括削除（hungry-delete）を有効にする
            (c-toggle-auto-hungry-state 1)
            (c-toggle-hungry-state 1)
            ;; タブストップリストの設定
            (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                                    64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
            ;; subwordモードの有効化（キャメルケースの単語をサブワードとして認識）
            (subword-mode 1))
          )

;;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; タブではなくスペースを用いたインデントを使用
            (setq indent-tabs-mode nil)
            ;; インデントを自動化
            (electric-indent-mode 1)
            ;; 対応する括弧をハイライト
            (show-paren-mode 1)
            ;; コメントやアンコメントの操作を容易にするキーバインドの設定
            (local-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
            ;; subwordモードの有効化（キャメルケースの単語をサブワードとして認識）
            (subword-mode 1))
          )
