;;; my-treesit.el --- tree-sitter 文法の配置先と導入経路 -*- lexical-binding: t; -*-
;;; Commentary:
;; C/C++ の tree-sitter 文法を var/package/tree-sitter/ へ導入するためのライブラリ。
;; 既定の ~/.emacs.d/tree-sitter/ (リポジトリ直下) は使わない。
;;
;; 導入経路は次の 2 つで、どちらも同じレシピ (固定タグ) と同じ導入先を使う。
;;   - M-x my/treesit-install-c-grammars      … C/C++ をまとめて導入する
;;   - M-x treesit-install-language-grammar   … Emacs 標準のコマンド
;;
;; use-package と straight に依存しないため、パッケージを 1 つも導入していない
;; 環境でも単体でロードできる (emacs-setup.sh --setup-treesit がこの形で使う)。
;;
;;   emacs --batch -l early-init.el -l loads/site-elisp/my-treesit.el \
;;     -f my/treesit-install-c-grammars
;;
;; 文法が無い環境では 19-language-modes.el の remap が成立せず cc-mode のまま動作する。
;; 自動ではインストールしない。

;;; Code:

;; treesit.el のロードは文法の導入時だけで足りるため require しない。
;; 下の 2 つは treesit.el 側の変数だが、defcustom も defvar も束縛済みの値を
;; 上書きしないため、ロード前に設定してよい。
(defvar treesit-language-source-alist)
(defvar treesit--install-language-grammar-out-dir-history)

(defvar my/treesit-grammar-dir (my-set-package "tree-sitter/")
  "tree-sitter 文法ライブラリの配置先。")

(defconst my/treesit-c-language-sources
  '((c   . ("https://github.com/tree-sitter/tree-sitter-c"   "v0.23.6" "src"))
    (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4" "src")))
  "C/C++ 文法の取得元。Emacs 30 が読める ABI へ収まるタグへ固定する。")

;; 標準の M-x treesit-install-language-grammar からも同じ固定タグで導入できるようにする
(setq treesit-language-source-alist my/treesit-c-language-sources)

;; 標準コマンドの導入先の既定値。`treesit-install-language-grammar' は OUT-DIR の
;; 既定をこの履歴の先頭から取るため、入れておかないと既定がリポジトリ直下になる。
;; 内部変数だが、消えても my/treesit-install-c-grammars は OUT-DIR を明示するため
;; 導入先そのものは変わらない。
(setq treesit--install-language-grammar-out-dir-history
      (list my/treesit-grammar-dir))

;; treesit-extra-load-path は treesit.c 側の変数で、treesit.el をロードせずに設定できる
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'treesit-extra-load-path my/treesit-grammar-dir))

(defun my/treesit-install-c-grammars (&optional force)
  "C/C++ の tree-sitter 文法を `my/treesit-grammar-dir' へ導入する.
FORCE (C-u) を付けると導入済みでも再ビルドする。git と C コンパイラが必要。
導入後の切り替えは Emacs の再起動で反映される。"
  (interactive "P")
  (unless (and (fboundp 'treesit-available-p) (treesit-available-p))
    (user-error "この Emacs は tree-sitter 無効ビルドです"))
  (require 'treesit)
  (make-directory my/treesit-grammar-dir t)
  (let ((failed nil))
    (dolist (entry my/treesit-c-language-sources)
      (let ((lang (car entry)))
        (if (and (not force) (treesit-language-available-p lang))
            (message "treesit: %s は導入済み" lang)
          (treesit-install-language-grammar lang my/treesit-grammar-dir)
          ;; treesit-install-language-grammar は失敗を display-warning で握り潰して
          ;; 正常終了する。batch 実行を fail-closed にするため自前で結果を検査する。
          (unless (treesit-language-available-p lang)
            (push lang failed)))))
    (when failed
      (error "treesit: 文法の導入に失敗しました (%s)"
             (mapconcat #'symbol-name (nreverse failed) ", "))))
  (message "treesit: 完了。Emacs を再起動すると ts モードへ切り替わります"))

(provide 'my-treesit)
;;; my-treesit.el ends here
