;;; 27-consult.el --- 検索・絞り込み・マッチングの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Consult, Embark, Orderless 等の検索・絞り込みパッケージの設定

;;; Code:

;; 遅延ロード先とプラットフォーム固有定義をコンパイラへ伝える。
(declare-function migemo-get-pattern "migemo")

;;; Consult - 多機能ミニバッファ補完
(use-package consult
  :straight t
  :after vertico
  :bind
  (("C-x f"   . consult-find)
   ("C-x C-r" . consult-recent-file)
   ("C-x C-y" . consult-yank-pop)
   ("C-x b"   . consult-buffer)
   ("C-x i"   . consult-imenu)
   ("C-s"     . consult-line)
   ;; 横断検索は C-S へ置けない。Emacs では C-S は C-s と同一のキーイベントであり
   ;; ((equal (kbd "C-s") (kbd "C-S")) は t)、:bind の後勝ちで C-s を奪ってしまう。
   ;; 端末が送出できる M-s l へ逃がして両方を到達可能にする。
   ("M-s l"   . my/consult-line-multi)
   ("C-."     . consult-goto-line)
   ("C-x g"   . my/consult-ripgrep-or-grep)
   ;; 見出し (C/C++ ts モードでは関数定義) へジャンプ。M-s o は Emacs 標準の occur
   ;; なので奪わず、consult 上流の推奨 M-g o を使う
   ("M-g o"   . consult-outline))
  :init
  ;; consult は遅延ロードのため、起動直後から xref 検索へ反映されるよう
  ;; :config でなく :init で設定する (defcustom は setq 済みの値を上書きしない)
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :custom-face
  ;; consult-file は font-lock-function-name-face を継承し斜体になるが、
  ;; Ricty Diminished Discord に italic バリアントがなく合成斜体でギザつくため無効化
  (consult-file ((t (:inherit font-lock-function-name-face :slant normal))))
  :config
  (defun my/consult-line-multi (&rest args)
    "1 文字から検索を開始する `consult-line-multi' ラッパー."
    (interactive "P")
    (let ((consult-async-min-input 1))
      (apply #'consult-line-multi args)))

  ;; consult-ripgrep は .gitignore を尊重する。ignore されたファイルも
  ;; 検索したいときは C-c g の生 grep を使う
  (defun my/consult-ripgrep-or-grep (&optional dir initial)
    "rg があれば consult-ripgrep、なければ consult-grep."
    (interactive "P")
    (if (executable-find "rg")
        (consult-ripgrep dir initial)
      (consult-grep dir initial))))

;;; consult-yasnippet - スニペットを補完一覧から選んで挿入
(use-package consult-yasnippet
  :straight t
  :bind ("C-c y" . consult-yasnippet))

;;; Embark - ミニバッファアクション
(use-package embark
  :straight t
  :bind
  (("M-a" . embark-act)))

;;; Embark-consult - `embark` と `consult` の連携
(use-package embark-consult
  :straight t
  ;; embark-collect でのプレビューは embark-consult 自身がフックへ登録する
  :after (embark consult))

;;; Orderless - 高度な補完フィルタ
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  :config
  ;; migemo を利用したローマ字検索
  (with-eval-after-load 'migemo
    (defun orderless-migemo (component)
      (when (>= (length component) 2)
        (let ((pattern (downcase (migemo-get-pattern component))))
          (condition-case nil
              (progn (string-match-p pattern "") pattern)
            (invalid-regexp nil)))))
    (add-to-list 'orderless-matching-styles 'orderless-migemo))

  ;; Corfu は orderless-flex で絞り込む。4 文字未満の単一語は前方一致を使う。
  (with-eval-after-load 'corfu
    (defun orderless-fast-dispatch (word index total)
      (and (= index 0) (= total 1) (length< word 4)
           'orderless-literal-prefix))

    (orderless-define-completion-style orderless-fast
      (orderless-style-dispatchers '(orderless-fast-dispatch))
      (orderless-matching-styles '(orderless-flex)))

    (defun my/setup-corfu-for-orderless ()
      ;; 補完スタイルだけを設定する。自動補完の待ち時間・文字数は 28-corfu.el で設定する。
      (setq-local completion-styles '(orderless-fast basic)))

    (add-hook 'corfu-mode-hook #'my/setup-corfu-for-orderless)))

(provide '27-consult)
;;; 27-consult.el ends here
