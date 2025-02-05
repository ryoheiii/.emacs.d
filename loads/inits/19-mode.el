;;; hs-minor-mode（関数折りたたみ）の設定
(use-package hideshow
  :ensure nil
  :bind (("C-\\" . hs-toggle-hiding)
         ("<f5>" . hs-toggle-hiding))
  :hook ((c-mode          . hs-minor-mode)
         (c++-mode        . hs-minor-mode)
         (emacs-lisp-mode . hs-minor-mode)
         (lisp-mode       . hs-minor-mode))
  )

;;; emacs-lisp-mode
(use-package elisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :ensure nil
  :hook (emacs-lisp-mode . my/emacs-lisp-mode-setup)
  :config
  (defun my/emacs-lisp-mode-setup ()
    "Emacs Lispモード用の設定"
    ;; タブではなくスペースを用いたインデントを使用
    (setq indent-tabs-mode nil)
    ;; インデントを自動化
    (electric-indent-mode 1)
    ;; 対応する括弧をハイライト
    (show-paren-mode 1)
    ;; コメントやアンコメントの操作を容易にするキーバインドの設定
    (local-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
    ;; subwordモードの有効化
    (subword-mode 1))
  )

;;; cc-mode の設定
(use-package cc-mode
  :mode (("\\.C\\'"    . c-mode)
         ;; ("\\.cc\\'"   . cc-mode)
         ("\\.cc\\'"   . c-mode)
         ("\\.nut\\'"  . c++-mode)
         ("\\.cpp\\'"  . c++-mode)
         ("\\.hh\\'"   . c++-mode)
         ("\\.c\\'"    . c-mode)
         ("\\.h\\'"    . c++-mode)
         ("\\.hpp\\'"  . c++-mode)
         ("\\.log\\'"  . c-mode)
         ("\\.cfg\\'"  . c-mode))
  :ensure nil
  :hook ((c-mode-common . my/cc-mode-setup)
         (c++-mode      . my/c++-mode-setup))
  :config
  (defun my/cc-mode-setup ()
    "C/C++ モード共通の設定"
    ;; コンパイル
    (local-set-key (kbd "C-c c") 'compile)
    ;; 自動改行と連続する空白の一括削除を有効にする
    (c-toggle-auto-hungry-state 1)
    ;; 他のエディタがファイルを書き換えたらすぐに反映する
    (auto-revert-mode)
    ;; インデント設定
    (setq indent-tabs-mode nil
          c-basic-offset 4))

  (defun my/c++-mode-setup ()
    "C++ モード固有の設定"
    )
  )

;;; text-mode
(use-package text-mode
  :mode (("\\.txt\\'" . text-mode)
         ("\\.tmp\\'" . text-mode)) ; コミットログ記載時にスニペットを展開したいため
  :ensure nil
  :hook (text-mode . my/text-mode-setup)
  :config
  (defun my/text-mode-setup ()
    "テキストモード用の設定"
    ;; インデント設定
    (setq indent-tabs-mode nil
          tab-width 2
          c-basic-offset 2)
    ;; subwordモードの有効化
    (subword-mode 1))
  )

;;; txt-mode
;; txt-mode-common-hook の設定（カスタムフック）
(add-hook 'txt-mode-common-hook
          (lambda ()
            ;; インデント設定
            (setq indent-tabs-mode nil
                  tab-width 2
                  c-indent-level 2
                  c-basic-offset 2)
            ;; 自動改行と連続する空白の一括削除を有効にする
            (c-toggle-auto-hungry-state 1)
            (c-toggle-hungry-state 1)
            ;; タブストップリストの設定
            (setq tab-stop-list (number-sequence 4 120 4)) ; 4 8 12 16...
            ;; subwordモードの有効化
            (subword-mode 1))
          )
