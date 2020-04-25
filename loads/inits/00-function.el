;; consider (local-set-key "\C-c\C-p" 'insert-dbgprint)
(defun my/insert-dbgprint (start end)
  "inserts the dbgprintf() sentence"
  (interactive "r")
  (let (result name value (case-fold-search-bak case-fold-search))
    (setq case-fold-search nil)
    (save-excursion
      (goto-char start)
      (goto-char (point-at-bol))
      (while (< (point) end)
        (when (looking-at "^\[ \t]*\\([A-Za-z][^ \t]+\\)[ \t]*=[ \t]*\\(.+?\\);")
          (setq name (buffer-substring (match-beginning 1) (match-end 1)))
          (setq value (buffer-substring (match-beginning 2) (match-end 2)))
          (setq result (concat result
                               (format "dbgprintf(\"%s = %%d" name)
                               (when (string-match "^[A-Z]" value)
                                 (format " (%s)" value))
                               (format "\\r\\n\", %s);\n" name)
                               )))
        (next-line)))
    (insert-before-markers result)
        (setq case-fold-search case-fold-search-bak)))

;;; 選択範囲をisearch
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;;; nlinum.el 関係
(defadvice linum-schedule (around my-linum-schedule () activate)
    (run-with-idle-timer 0.2 nil #'linum-update-current))

;;; window関連
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))

;; window リサイズ
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
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
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
                              (throw 'end-flag t)))))))
