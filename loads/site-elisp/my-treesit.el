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

(defun my/treesit-register-c-sources ()
  "C/C++ のレシピを `treesit-language-source-alist' へ登録する.
標準の \\[treesit-install-language-grammar] からも同じ固定タグで導入できるようにする。
既存の登録は残す（丸ごと `setq' すると custom.el などが登録した他言語が消える）。"
  (unless (boundp 'treesit-language-source-alist)
    (setq treesit-language-source-alist nil))
  (dolist (entry my/treesit-c-language-sources)
    (setf (alist-get (car entry) treesit-language-source-alist) (cdr entry))))

(my/treesit-register-c-sources)

;; 標準コマンドの導入先の既定値。`treesit-install-language-grammar' は OUT-DIR の
;; 既定をこの履歴の先頭から取るため、入れておかないと既定がリポジトリ直下になる。
;; 内部変数だが、消えても my/treesit-install-c-grammars は OUT-DIR を明示するため
;; 導入先そのものは変わらない。
(setq treesit--install-language-grammar-out-dir-history
      (list my/treesit-grammar-dir))

;; treesit-extra-load-path は treesit.c 側の変数で、treesit.el をロードせずに設定できる
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'treesit-extra-load-path my/treesit-grammar-dir))

(defun my/treesit-grammars-installable-p ()
  "この Emacs で文法をビルド・導入できるなら non-nil.
`treesit-install-language-grammar' が OUT-DIR を受け取るのは Emacs 30 以降で、
29 以前は導入先を指定できない（既定のリポジトリ直下へ書き出してしまう）。
可用性は `func-arity' で見る。autoload のままでも解決できるため treesit.el を
ロードせずに判定できる。"
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (fboundp 'treesit-install-language-grammar)
       (>= (cdr (func-arity 'treesit-install-language-grammar)) 2)))

(defun my/treesit-install-one-grammar (lang)
  "LANG の文法をビルドして `my/treesit-grammar-dir' へ導入する.
成功したら non-nil を返す。

`treesit-install-language-grammar' は clone・ビルド・検査のいずれの失敗も
`display-warning' で報告して正常終了するため、戻り値では成否が分からない。
さらに再ビルド時は古い文法が残っているため `treesit-language-available-p' も
真のままになる。今回の実行で警告が出たかどうかを併せて見る。"
  (require 'cl-lib)
  (let ((warned nil))
    (cl-letf* ((orig (symbol-function 'display-warning))
               ((symbol-function 'display-warning)
                (lambda (type message &rest args)
                  (when (eq (if (consp type) (car type) type) 'treesit)
                    (setq warned t))
                  (apply orig type message args))))
      (treesit-install-language-grammar lang my/treesit-grammar-dir))
    (and (not warned) (treesit-language-available-p lang))))

(defun my/treesit-install-c-grammars (&optional force)
  "C/C++ の tree-sitter 文法を `my/treesit-grammar-dir' へ導入する.
FORCE (C-u) を付けると導入済みでも再ビルドする。git と C コンパイラが必要。
導入後の切り替えは Emacs の再起動で反映される。"
  (interactive "P")
  (unless (and (fboundp 'treesit-available-p) (treesit-available-p))
    (user-error "この Emacs は tree-sitter 無効ビルドです"))
  (unless (my/treesit-grammars-installable-p)
    (user-error "導入先を指定できる Emacs 30 以降が必要です (現在 %s)" emacs-version))
  (require 'treesit)
  (make-directory my/treesit-grammar-dir t)
  (let ((failed nil))
    (dolist (entry my/treesit-c-language-sources)
      (let ((lang (car entry)))
        (if (and (not force) (treesit-language-available-p lang))
            (message "treesit: %s は導入済み" lang)
          (unless (my/treesit-install-one-grammar lang)
            (push lang failed)))))
    (when failed
      (error "treesit: 文法の導入に失敗しました (%s)"
             (mapconcat #'symbol-name (nreverse failed) ", "))))
  (message "treesit: 完了。Emacs を再起動すると ts モードへ切り替わります"))

(provide 'my-treesit)
;;; my-treesit.el ends here
