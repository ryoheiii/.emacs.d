;;; 19-language-modes.el --- 各種モード設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; 主要なメジャーモードの設定を行う

;;; Code:

;;;;;; [Group] Code Folding - コード折りたたみ ;;;;;;
(use-package hideshow
  :straight nil
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
  :straight nil
  :hook (emacs-lisp-mode . my/emacs-lisp-mode-setup)
  :config
  (defun my/emacs-lisp-mode-setup ()
    "Emacs Lisp モード用の設定。"
    (setq indent-tabs-mode nil)  ; タブではなくスペースを使用
    (subword-mode 1))            ; CamelCase も単語として移動
  )

;;; Cc-mode - cc-mode の設定
(use-package cc-mode
  :straight nil
  :mode (("\\.C\\'"    . c-mode)
         ("\\.cc\\'"   . c++-mode)
         ("\\.nut\\'"  . c++-mode)
         ("\\.cpp\\'"  . c++-mode)
         ("\\.hh\\'"   . c++-mode)
         ("\\.c\\'"    . c-mode)
         ("\\.h\\'"    . c++-mode)
         ("\\.hpp\\'"  . c++-mode)
         ("\\.log\\'"  . c-mode)
         ("\\.cfg\\'"  . c-mode))
  :hook (c-mode-common . my/cc-mode-setup)
  :config
  (defun my/cc-mode-setup ()
    "C/C++ モード共通の設定。"
    (local-set-key (kbd "C-c c") 'compile) ; コンパイル
    (c-toggle-auto-hungry-state 1)         ; 自動改行 & 連続スペース一括削除
    (setq indent-tabs-mode nil
          c-basic-offset 4))
  )

;;;;;; [Group] Text Editing - テキスト編集 ;;;;;;
(use-package text-mode
  :straight nil
  :mode (("\\.txt\\'" . text-mode)
         ("\\.tmp\\'" . text-mode))
  :hook (text-mode . my/text-mode-setup)
  :config
  (defun my/text-mode-setup ()
    "テキストモード用の設定。ただし markdown-mode では適用しない。"
    (unless (derived-mode-p 'markdown-mode) ;; markdown-mode では適用しない
      (setq indent-tabs-mode nil
            tab-width 2)
      (subword-mode 1))) ; CamelCase も単語として移動
  )

(provide '19-language-modes)
;;; 19-language-modes.el ends here
