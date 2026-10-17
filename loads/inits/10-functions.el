;;; 10-functions.el --- ユーティリティ関数集 -*- lexical-binding: t; -*-
;;; Commentary:
;; パッケージ判定、ファイル名のコピー、ウィンドウ操作の共通関数。

;;; Code:

;; 遅延ロード先とプラットフォーム固有定義をコンパイラへ伝える。
(declare-function package-built-in-p "package")

;;;;; [Group] `straight.el` 判定 ;;;;;
(defun my/should-use-straight (package)
  "指定した PACKAGE が `straight.el` で管理すべきかを判定する関数。
`t` なら `straight t`、`nil` なら `straight nil` を追加すべき。"
  (interactive
   (list (intern (completing-read "Package name: " obarray))))
  (require 'package)
  (let* ((library (locate-library (symbol-name package)))
         ;; lisp-directory はコアライブラリの配置先を指す組み込み変数。
         (is-built-in (or (package-built-in-p package)
                          (and library (file-in-directory-p library lisp-directory)))))
    (if is-built-in
        (progn
          (message "Package '%s' is built-in. `straight nil` is recommended." package)
          nil)
      (progn
        (message "Package '%s' is NOT built-in. `straight t` is recommended." package)
        t))))

;;;;; [Group] ファイル関連 ;;;;;
;;; ファイルパス・ファイル名をキルリングへ保存
(defun my/copy-to-clipboard-and-message (text message)
  "TEXT をキルリングへ保存し、ミニバッファに MESSAGE を表示する。"
  (kill-new text)
  (message "%s" message))

(defun my/copy-file-path ()
  "現在のファイルの絶対パスをキルリングへ保存し、ミニバッファに表示する。"
  (interactive)
  (when buffer-file-name
    (my/copy-to-clipboard-and-message (file-truename buffer-file-name)
                                      (format "Copied file path: %s" buffer-file-name))))

(defun my/copy-file-name ()
  "現在のファイル名をキルリングへ保存し、ミニバッファに表示する。"
  (interactive)
  (when buffer-file-name
    (let ((file-name (file-name-nondirectory buffer-file-name)))
      (my/copy-to-clipboard-and-message file-name
                                        (format "Copied file name: %s" file-name)))))

;;;;; [Group] ウィンドウ管理 ;;;;;
;; 上下に分割
(defun my/split-window-vertically-n (num_wins)
  "現在のウィンドウを上下に NUM_WINS 個へ分割する。"
  (interactive (list (if current-prefix-arg
                         (prefix-numeric-value current-prefix-arg) 2)))
  (unless (and (integerp num_wins) (>= num_wins 2))
    (user-error "分割数は 2 以上の整数を指定してください"))
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (my/split-window-vertically-n (- num_wins 1)))))

;; 左右に分割
(defun my/split-window-horizontally-n (num_wins)
  "現在のウィンドウを左右に NUM_WINS 個へ分割する。"
  (interactive (list (if current-prefix-arg
                         (prefix-numeric-value current-prefix-arg) 2)))
  (unless (and (integerp num_wins) (>= num_wins 2))
    (user-error "分割数は 2 以上の整数を指定してください"))
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (my/split-window-horizontally-n (- num_wins 1)))))

;; 他のウィンドウへ移動、または新規分割
(defun my/other-window-or-split ()
  "次のウィンドウへ移動する。1 枚しかない場合は先に左右へ分割する。"
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (my/split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))

;; ウィンドウリサイズ機能
(defun my/window-resizer ()
  "キー入力で現在のウィンドウの大きさを調整する。
[f] → 増加, [b] ← 減少, [n] ↓ 増加, [p] ↑ 減少, 他のキーで終了。"
  (interactive)
  (let ((dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
        c)
    (catch 'end-flag
      (while t
        (setq c (read-key "Resize: [f]→ [b]← [n]↓ [p]↑, any other key to exit"))
        (cond
         ;; ウィンドウが1つだけならリサイズしない
         ((one-window-p)
          (message "Cannot resize: only one window exists.")
          (throw 'end-flag t))
         ;; 横幅変更
         ((eq c ?f) (ignore-errors (enlarge-window-horizontally dx)))
         ((eq c ?b) (ignore-errors (shrink-window-horizontally dx)))
         ;; 高さ変更
         ((eq c ?n) (ignore-errors (shrink-window dy)))
         ((eq c ?p) (ignore-errors (enlarge-window dy)))
         ;; それ以外のキーなら終了
         (t
          (message "Quit")
          (throw 'end-flag t)))))))

(provide '10-functions)
;;; 10-functions.el ends here
