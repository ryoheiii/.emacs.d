;;; 19-mode.el --- 各種モード設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; 主要なメジャーモードの設定を行う

;;; Code:

;;;;;; [Group] Code Folding - コード折りたたみ ;;;;;;
(use-package hideshow
  :bind (("C-\\" . hs-toggle-hiding)
         ("<f5>" . hs-toggle-hiding))
  :hook ((c-mode          . hs-minor-mode)
         (c++-mode        . hs-minor-mode)
         (emacs-lisp-mode . hs-minor-mode)
         (lisp-mode       . hs-minor-mode))
  )

;;;;;; [Group] Programming Modes - プログラミングモード ;;;;;;
;;; Elisp-mode - elisp-mode の設定
(use-package elisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :hook (emacs-lisp-mode . my/emacs-lisp-mode-setup)
  :config
  (defun my/emacs-lisp-mode-setup ()
    "Emacs Lisp モード用の設定。"
    (setq indent-tabs-mode nil)  ; タブではなくスペースを使用
    (electric-indent-mode 1)     ; インデントを自動化
    (show-paren-mode 1)          ; 括弧の対応をハイライト
    (local-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
    (subword-mode 1))            ; CamelCase も単語として移動
  )

;;; Cc-mode - cc-mode の設定
(use-package cc-mode
  :mode (("\\.C\\'"    . c-mode)
         ;; ("\\.cc\\'"   . cc-mode)
         ("\\.cc\\'"   . c-mode)  ;; cc-mode ではなく c-mode に統一
         ("\\.nut\\'"  . c++-mode)
         ("\\.cpp\\'"  . c++-mode)
         ("\\.hh\\'"   . c++-mode)
         ("\\.c\\'"    . c-mode)
         ("\\.h\\'"    . c++-mode)
         ("\\.hpp\\'"  . c++-mode)
         ("\\.log\\'"  . c-mode)
         ("\\.cfg\\'"  . c-mode))
  :hook ((c-mode-common . my/cc-mode-setup)
         (c++-mode      . my/c++-mode-setup))
  :config
  (defun my/cc-mode-setup ()
    "C/C++ モード共通の設定。"
    (local-set-key (kbd "C-c c") 'compile)  ; コンパイル
    (c-toggle-auto-hungry-state 1)          ; 自動改行 & 連続スペース一括削除
    (auto-revert-mode)                      ; 外部変更の即時反映
    (setq indent-tabs-mode nil
          c-basic-offset 4))

  (defun my/c++-mode-setup ()
    "C++ モード固有の設定。"
    ;; 追加設定があればここに記述
    )
  )

;;;;;; [Group] Text Editing - テキスト編集 ;;;;;;
(use-package text-mode
  :mode (("\\.txt\\'" . text-mode)
         ("\\.tmp\\'" . text-mode))
  :hook (text-mode . my/text-mode-setup)
  :config
  (defun my/text-mode-setup ()
    "テキストモード用の設定。"
    (setq indent-tabs-mode nil
          tab-width 2
          c-basic-offset 2)
    (setq tab-stop-list (number-sequence 4 120 4)) ; タブストップを 4 の倍数で設定
    (subword-mode 1))                              ; CamelCase も単語として移動
  )

(provide '19-mode)
;;; 19-mode.el ends here
