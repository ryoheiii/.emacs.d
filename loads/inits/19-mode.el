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
         ("\\.cc\\'"   . cc-mode)
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

(use-package org
  :ensure t
  :defer t
  :hook ((org-mode . visual-line-mode)   ;; 自動改行の有効化
         (org-mode . org-indent-mode))   ;; インデントを自動調整
  :custom
  (org-hide-leading-stars t)             ;; 見出しの*を非表示
  (org-startup-indented t)               ;; インデント表示をデフォルトで有効化
  (org-src-fontify-natively t)           ;; ソースコードをシンタックスハイライト
  (org-src-tab-acts-natively t)          ;; org-babelでタブキーを言語モードに連動
  (org-edit-src-content-indentation 2)   ;; org-babelのソースコードインデント
  (org-startup-folded 'content)          ;; 初期表示で折りたたむ
  (org-log-done 'time)                   ;; タスク完了時に時間を記録
  (org-log-into-drawer t)                ;; ログを :LOGBOOK: に格納
  (org-adapt-indentation nil)            ;; インデントの自動調整をオフにする
  (org-cycle-separator-lines 2)          ;; 見出しの間隔
  (org-ellipsis " ▼")                   ;; 折りたたみ表示の記号変更
  :config
  (setq org-agenda-files '("~/org/agenda/"))  ;; アジェンダファイルのディレクトリ指定
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  )
