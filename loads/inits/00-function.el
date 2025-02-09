;;; 00-function.el --- ユーティリティ関数集 -*- lexical-binding: t; -*-
;;; Commentary:
;; 各種ユーティリティ関数を定義

;;; Code:

;;;;;; [Group] Package Management - `straight.el` 判定 ;;;;;;
;;; 指定した PACKAGE が `straight.el` で管理すべきかを判定する関数
(defun my/should-use-straight (package)
  "指定した PACKAGE が `straight.el` で管理すべきかを判定する関数。
`t` なら `straight t` を追加すべき、`nil` なら不要。"
  (interactive
   (list (intern (completing-read "Package name: " (mapcar #'symbol-name package-activated-list)))))
  (if (or (featurep package) (locate-library (symbol-name package)))
      (progn
        (message "Package '%s' is built-in. `straight t` is NOT needed." package)
        nil)
    (progn
      (message "Package '%s' is NOT built-in. `straight t` is recommended." package)
      t)))

;;;;;; [Group] File Operations - ファイル関連 ;;;;;;
;;; ファイルパス/ファイル名をクリップボードに保存
(defun my/copy-to-clipboard-and-message (text message)
  "Copy TEXT to the kill ring and display MESSAGE in the minibuffer."
  (kill-new text)
  (message message))

(defun my/copy-file-path ()
  "Show the full path file name in the minibuffer and copy to kill ring."
  (interactive)
  (when buffer-file-name
    (my/copy-to-clipboard-and-message (file-truename buffer-file-name)
                                      (format "Copied file path: %s" buffer-file-name))))

(defun my/copy-file-name ()
  "Show the file name in the minibuffer and copy it to the kill ring."
  (interactive)
  (when buffer-file-name
    (let ((file-name (file-name-nondirectory buffer-file-name)))
      (my/copy-to-clipboard-and-message file-name
                                        (format "Copied file name: %s" file-name)))))

;;;;;; [Group] Debugging - デバッグ関連 ;;;;;;
;;; デバッグプリントの挿入
(defun my/insert-dbgprint (start end)
  "選択範囲内の変数代入文にデバッグ出力文 (dbgprintf) を挿入する

   この関数は変数宣言と代入の両方にマッチし、各行に対して dbgprintf 文を生成して挿入する。
   変数名は英字、アンダースコア、数字から成り、出力形式は整数を想定している。

   PARAMETERS:
     start (int): 選択範囲の開始位置
     end (int)  : 選択範囲の終了位置

   使用方法:
     C 言語コード内でデバッグ出力を挿入したい範囲を選択し、この関数を実行する。"
  (interactive "r")
  (let ((result "")  ; Initialize result as an empty string
        name
        value)
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (not (eobp)))
        ;; Match variable declarations with initialization and simple assignments
        (when (looking-at "^[ \t]*\\(?:[A-Za-z_][A-Za-z0-9_]*[ \t]+\\)?\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t]*=[ \t]*\\([^;\n]+\\);")
          ;; Capture variable name and value from the line
          (setq name (match-string-no-properties 1)
                value (match-string-no-properties 2))
          ;; Construct the debug print statement
          (setq result (concat result
                               (format "dbgprintf(\"%s = %%d\\r\\n\", %s);\n" name name))))
        (forward-line 1)))  ; Move to the next line
    (goto-char end)
    (insert result)))  ; Insert the result into the buffer


;;;;;; [Group] Window Management - ウィンドウ管理 ;;;;;;
;;; ウィンドウ関連操作
;; 垂直分割
(defun split-window-vertically-n (num_wins)
  "ウィンドウを垂直方向に NUM_WINS 分割。"
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))

;; 水平分割
(defun split-window-horizontally-n (num_wins)
  "ウィンドウを水平方向に NUM_WINS 分割。"
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

;; 他のウィンドウへ移動、または新規分割
(defun other-window-or-split ()
  "他のウィンドウへ移動、または新規分割。"
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))

;; ウィンドウリサイズ機能
(defun window-resizer ()
  "Control window size and position.
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
         ((= c ?f) (ignore-errors (enlarge-window-horizontally dx)))
         ((= c ?b) (ignore-errors (shrink-window-horizontally dx)))
         ;; 高さ変更
         ((= c ?n) (ignore-errors (shrink-window dy)))
         ((= c ?p) (ignore-errors (enlarge-window dy)))
         ;; それ以外のキーなら終了
         (t
          (message "Quit")
          (throw 'end-flag t)))))))

(provide '00-function)
;;; 00-function.el ends here
