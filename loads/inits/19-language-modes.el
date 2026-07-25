;;; 19-language-modes.el --- 各種モード設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; 主要なメジャーモードの設定を行う

;;; Code:

;;;;;; [Group] Code Folding - コード折りたたみ ;;;;;;
(use-package hideshow
  :straight nil
  :bind (("C-\\" . hs-toggle-hiding)
         ("<f5>" . hs-toggle-hiding))
  ;; ts モードは c-mode/c++-mode のフックを継承しないため個別に登録する
  ;; (hs-special-modes-alist は c-ts-mode / c++-ts-mode のエントリを持つ)
  :hook ((c-mode          . hs-minor-mode)
         (c++-mode        . hs-minor-mode)
         (c-ts-mode       . hs-minor-mode)
         (c++-ts-mode     . hs-minor-mode)
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

;;; C-ts-mode - tree-sitter 版 C/C++ モード (段階移行)
;; 文法が導入済みなら c-ts-mode / c++-ts-mode、無ければ従来の cc-mode を使う。
;; 文法をビルドできない環境では何も起きず cc-mode のまま動作する（フォールバック）。
;; 文法の導入は 18-built-in-package.el の M-x my/treesit-install-c-grammars で行う。
;;
;; 既知の使用感差分: c-toggle-auto-hungry-state 相当が ts モードには存在しないため、
;; 自動改行と連続スペース一括削除は ts モードでは無効になる。
(defvar my/use-treesit-for-cc t
  "Non-nil なら文法が揃っている C/C++ で ts モードを使う。
nil にすると文法があっても cc-mode を使い続ける。")

(defconst my/cc-non-source-regexp "\\.\\(log\\|cfg\\|nut\\)\\'"
  "cc-mode を流用しているだけで C/C++ ではないファイル。
.log/.cfg はログ閲覧用の流用、.nut は Squirrel である。ts モードへ回すと
バッファ全体が ERROR ノードになりフォントロックを失うため remap 対象から除外する。")

(defun my/cc-real-source-p ()
  "現在のバッファが ts モードへ回してよい実 C/C++ ソースなら non-nil."
  (and buffer-file-name
       (not (string-match-p my/cc-non-source-regexp buffer-file-name))))

(defun my/treesit-cc-grammar-ready-p (lang)
  "LANG の文法が使える状態なら non-nil.
treesit-language-available-p は treesit.el をロードせず警告も出さない."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-available-p lang)))

(defun my/treesit-cc-ready-p (lang)
  "LANG で ts モードを使ってよいなら non-nil."
  (and my/use-treesit-for-cc (my/treesit-cc-grammar-ready-p lang)))

;; auto-mode から呼ばれる振り分け関数（組み込みの c-or-c++-mode と同じ形）。
;; 可用性はここで毎回判定するため、`my/use-treesit-for-cc' を後から nil にしても効く。
(defun my/c-mode-dispatch ()
  "実 C ソースなら `c-ts-mode'、流用ファイルなら `c-mode' を有効にする."
  (interactive)
  (if (and (my/treesit-cc-ready-p 'c) (my/cc-real-source-p))
      (c-ts-mode)
    (c-mode)))

(defun my/c++-mode-dispatch ()
  "実 C++ ソースなら `c++-ts-mode'、流用ファイルなら `c++-mode' を有効にする."
  (interactive)
  (if (and (my/treesit-cc-ready-p 'cpp) (my/cc-real-source-p))
      (c++-ts-mode)
    (c++-mode)))

(defconst my/treesit-cc-remap-entries
  '((c-mode   c   my/c-mode-dispatch)
    (c++-mode cpp my/c++-mode-dispatch))
  "(cc-mode 側のモード 文法 振り分け関数) の対応。")

(defun my/treesit-cc-apply-remap ()
  "文法が使える言語だけ `major-mode-remap-alist' へ振り分け関数を登録する.
C と C++ は独立に判定する（片方の文法だけある環境でも壊さない）。
`my/use-treesit-for-cc' が nil でも登録する。c-ts-mode.el はロード時に
`major-mode-remap-defaults' を書き換えるため、登録しておかないと off が
迂回されるうえ、.log/.cfg のような流用ファイルまで ts へ回ってしまう。
`major-mode-remap-alist' は defaults より優先されるため、ここでの指定が常に勝つ。"
  (pcase-dolist (`(,mode ,lang ,dispatch) my/treesit-cc-remap-entries)
    (if (my/treesit-cc-grammar-ready-p lang)
        (setf (alist-get mode major-mode-remap-alist) dispatch)
      (setq major-mode-remap-alist
            (assq-delete-all mode major-mode-remap-alist)))))

(my/treesit-cc-apply-remap)

(use-package c-ts-mode
  :straight nil
  ;; 起動経路で require しない（文法不在時に treesit の警告が出るため）
  :defer t
  ;; use-package はバイトコンパイル時にパッケージを先読みする。c-ts-mode.el は
  ;; ロード時に treesit-ready-p を呼ぶため、先読みすると make lint が警告で汚れる。
  :no-require t
  :hook ((c-ts-mode   . my/c-ts-mode-setup)
         (c++-ts-mode . my/c-ts-mode-setup))
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style #'my/c-ts-mode-indent-style)
  :init
  (defun my/c-ts-mode-setup ()
    "C/C++ ts モード共通の設定（cc-mode 側の `my/cc-mode-setup' と対応）."
    (local-set-key (kbd "C-c c") 'compile) ; コンパイル
    (setq indent-tabs-mode nil))

  (defun my/c-ts-mode-indent-style ()
    "k&r をベースに google-c-style 相当の差分を前置した indent 規則を返す.
前置した規則が先に照合されるためベース側の同種規則を上書きできる。
ベース取得に使う `c-ts-mode--indent-styles' は内部関数のため、
失われた場合でも差分規則だけで動作を継続する（エラーにしない）。"
    (let* ((offset (if (boundp 'c-ts-mode-indent-offset) c-ts-mode-indent-offset 4))
           (half (/ offset 2))
           (base (when (fboundp 'c-ts-mode--indent-styles)
                   (alist-get 'k&r (c-ts-mode--indent-styles
                                    (if (derived-mode-p 'c++-ts-mode) 'cpp 'c))))))
      (append
       `(;; google-c-style の (innamespace . 0): namespace 本体をインデントしない
         ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)
         ;; google-c-style の (access-label . /): public: 等をメンバより半段浅く置く
         ((node-is "access_specifier") parent-bol ,half)
         ;; google-c-style の (case-label . +): case を switch から 1 段下げる
         ((node-is "case_statement") standalone-parent ,offset))
       base)))
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
