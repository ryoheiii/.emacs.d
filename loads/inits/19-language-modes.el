;;; 19-language-modes.el --- 各種モード設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; 主要なメジャーモードの設定を行う

;;; Code:

;; 遅延ロード先とプラットフォーム固有定義をコンパイラへ伝える。
(declare-function c-toggle-auto-hungry-state "cc-cmds")
(declare-function outline-cycle "outline")
(declare-function outline-cycle-buffer "outline")
(declare-function outline-back-to-heading "outline")
(defvar c-ts-indent-offset)
(defvar treesit-outline-predicate)

;;;;;; [Group] Code Folding - コード折りたたみ ;;;;;;
(use-package hideshow
  :straight nil
  :bind (("C-\\" . hs-toggle-hiding)
         ("<f5>" . hs-toggle-hiding))
  ;; ts モードは c-mode/c++-mode のフックを継承しないため個別に登録する
  ;; (ts モードの hideshow は treesit-major-mode-setup が list thing で設定する。
  ;;  hs-special-modes-alist は 31.1 で obsolete)
  :hook ((c-mode          . hs-minor-mode)
         (c++-mode        . hs-minor-mode)
         (c-ts-mode       . hs-minor-mode)
         (c++-ts-mode     . hs-minor-mode)
         (emacs-lisp-mode . hs-minor-mode)
         (lisp-mode       . hs-minor-mode))
  )

;;; Outline - 関数単位の折りたたみと一覧 (hideshow と併用)
;; treesit 連動の見出し (関数定義。述語は my/c-ts-outline-predicate) が取れる ts モード
;; だけで有効にする。cc-mode の既定は outline-regexp "[^#\n\^M]" と c-outline-level
;; (インデント桁) で「# 以外の全行が見出し」になり、consult-outline が全行を列挙し
;; outline-cycle は桁 0 の行でしか意味を持たないため、cc-mode では現状 (hideshow のみ)
;; を維持する。ts モードのフック parity 規約の意図的な例外で、
;; tests/my-test-cpp-config.el の my-test-cpp-config-outline-hook-ts-only が固定する。
;; 既定の見た目・入力は変えない: outline-minor-mode-cycle (TAB の乗っ取り) と
;; outline-minor-mode-highlight は既定の nil のまま。
;; hideshow (<f5>: 内側のブロック、invisible 'hs) と outline (<f9>: 関数、
;; invisible 'outline) は独立した overlay 層で、両方で隠した領域は片方の show では開かない。
(use-package outline
  :straight nil
  :hook ((c-ts-mode   . outline-minor-mode)
         (c++-ts-mode . outline-minor-mode))
  :bind (("<f9>" . my/outline-cycle-dwim))
  ;; :bind から参照する関数は :preface で定義する (:init だと多重定義警告が出る)
  :preface
  (defun my/outline-cycle-dwim (&optional arg)
    "カーソル位置の関数の折りたたみを切り替える.
本体の中からでも見出し (関数定義行) へ point を移してから `outline-cycle' を呼ぶ
(`outline-cycle' 単体は point を不可視領域に残す)。ARG (C-u) 付きは
`outline-cycle-buffer' でバッファ全体を切り替える。`outline-minor-mode' が無効な
バッファ (cc-mode など) では従来どおり `hs-toggle-hiding' を呼ぶ。"
    (interactive "P")
    (cond
     ((not (bound-and-true-p outline-minor-mode))
      (call-interactively #'hs-toggle-hiding))
     (arg (outline-cycle-buffer))
     (t
      (condition-case nil
          (outline-back-to-heading t)
        (outline-before-first-heading
         (user-error "この位置より前に関数定義がありません")))
      (outline-cycle))))
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
    (keymap-local-set "C-c c" 'compile)    ; コンパイル
    (c-toggle-auto-hungry-state 1)         ; 自動改行 & 連続スペース一括削除
    (setq indent-tabs-mode nil
          c-basic-offset 4))
  )

;;; C-ts-mode - tree-sitter 版 C/C++ モード (段階移行)
;; 文法が導入済みなら c-ts-mode / c++-ts-mode、無ければ従来の cc-mode を使う。
;; 文法をビルドできない環境では何も起きず cc-mode のまま動作する（フォールバック）。
;; 文法の導入は 18-built-in-package.el の M-x my/treesit-install-c-grammars で行う。
;;
;; ts モードには c-toggle-auto-hungry-state が無いため、自動改行と連続スペース
;; 一括削除は組み込み機能で再現する（my/c-ts-mode-setup を参照）。
;; 波括弧が閉じていない入力途中は構文木が ERROR になるため、桁は括弧の深さから
;; 算出する（my/c-ts-error-context-p を参照）。構文が揃えば、ブレース初期化の
;; `{' 配置（意図的な差分。docs/cpp.md を参照）を除いて cc-mode + google-c-style
;; と一致する。
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
`my/use-treesit-for-cc' が nil でも登録する。振り分け関数が毎回判定するため、
後から t/nil を切り替えても再起動なしで効き、.log/.cfg のような流用ファイルは
cc-mode に留まる。c-ts-mode.el はロード時に `treesit-major-mode-remap-alist' へ
追記するだけで、`treesit-enabled-modes' が nil (既定) なら
`major-mode-remap-alist' へは何も入らない。`treesit-enabled-modes' へ C/C++ を
含めると (c-mode . c-ts-mode) が先頭へ入りこの振り分けを覆うため、含めないこと。"
  (pcase-dolist (`(,mode ,lang ,dispatch) my/treesit-cc-remap-entries)
    (if (my/treesit-cc-grammar-ready-p lang)
        (setf (alist-get mode major-mode-remap-alist) dispatch)
      (setq major-mode-remap-alist
            (assq-delete-all mode major-mode-remap-alist)))))

(my/treesit-cc-apply-remap)

(use-package c-ts-mode
  :straight nil
  ;; 起動経路で require しない（treesit.el まで読み込まれ起動が重くなるため）
  :defer t
  ;; use-package はバイトコンパイル時にパッケージを先読みする。参照は先頭の defvar と
  ;; declare-function で足りるため、treesit.el を含む先読みも止める。
  :no-require t
  :hook ((c-ts-mode   . my/c-ts-mode-setup)
         (c++-ts-mode . my/c-ts-mode-setup))
  :custom
  (c-ts-indent-offset 4)
  (c-ts-mode-indent-style 'k&r)
  ;; :hook から参照する関数は :preface で定義する (:init だと多重定義警告が出る)
  :preface
  (require 'my-c-ts-edit)
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
