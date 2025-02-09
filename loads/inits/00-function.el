(defun my/should-use-straight (package)
  "指定した PACKAGE が `straight.el` で管理すべきかを判定する関数。
 - `t` なら `straight t` を追加すべき
 - `nil` なら `straight t` は不要"
  (interactive
   (list (intern (completing-read "Package name: " (mapcar #'symbol-name (apropos-internal "" 'featurep))))))
  (if (or (featurep package) (locate-library (symbol-name package)))
      (progn
        (message "Package '%s' is built-in. `straight t` is NOT needed." package)
        nil)
    (progn
      (message "Package '%s' is NOT built-in. `straight t` is recommended." package)
      t)))

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

;;; 選択範囲を isearch
(defadvice isearch-mode (around isearch-mode-default-string
                                (forward &optional regexp op-fun recursive-edit word-p) activate)
  "Isearch with default text if there is a selection."
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (let ((search-text (buffer-substring-no-properties (mark) (point))))
        (isearch-update-ring search-text)
        (deactivate-mark)
        ad-do-it
        (if forward
            (progn
              (goto-char (mark))
              (isearch-repeat-forward))
          (isearch-repeat-backward)))
    ad-do-it))


;;; nlinum.el の遅延更新
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))

;;; ウィンドウ関連操作
;; 垂直分割
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))

;; 水平分割
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

;; 他のウィンドウへ移動、または新規分割
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))

;; ウィンドウリサイズ機能
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action (read-key-sequence-vector (format "size[%dx%d]" (window-width) (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?f) (enlarge-window-horizontally dx))
              ((= c ?b) (shrink-window-horizontally dx))
              ((= c ?n) (enlarge-window dy))
              ((= c ?p) (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0)) (command (key-binding action)))
                 (when command (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))
