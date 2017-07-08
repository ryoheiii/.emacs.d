;;; 文字列
;;; 2012-03-24

;;; moveline
(defun move-line (arg)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines arg))
    (when (> arg 0)
      (forward-line arg))
    (move-to-column col)))

(global-set-key (kbd "C-M-n") (lambda () (interactive) (move-line 1)))
(global-set-key (kbd "C-M-p") (lambda () (interactive) (move-line -1)))

;; 文字列の先頭と最後にある一連の空白をすべて削除する
;; 2012-03-24
(defun string-strip (string)
  (replace-regexp-in-string "\\`[ \r\n]*\\|[ \r\n]*\\'" "" string))


;;; パス
;;; 2011-03-19
;; directoryの中にbase-names内のパスが含まれていたらその絶対パスを返す。
;; 含まれていなかったらdirectoryの親のディレクトリを再帰的に探す。
;; 2011-03-19
(defun find-path-in-parents (directory base-names)
  (or (find-if 'file-exists-p
               (mapcar (lambda (base-name)
                         (concat directory base-name))
                       base-names))
      (if (string= directory "/")
          nil
        (let ((parent-directory (substring directory 0 -1)))
          (find-path-in-parents parent-directory base-names)))))

;;; window resize
(global-set-key "\C-c\C-r" 'my-window-resizer)
(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?f)
               (enlarge-window-horizontally dx))
              ((= c ?b)
               (shrink-window-horizontally dx))
              ((= c ?n)
               (enlarge-window dy))
              ((= c ?p)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))


