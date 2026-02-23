;;; my-gtags.el --- ggtags カスタム関数群 -*- lexical-binding: t; -*-
;;; Commentary:
;; ggtags の xref バックエンドを経由せず、global コマンドを call-process で直接実行し
;; 結果を consult-xref → vertico で高速表示する関数群

;;; Code:

(require 'xref)

;; ── global 直接実行エンジン ──────────────────────────────
;; ggtags の xref バックエンド・プロセス管理・プロジェクト検証を経由せず
;; call-process で global を直接実行し、最短パスで結果を得る

(defun my/gtags--project-root ()
  "GTAGS ファイルを探索してプロジェクトルートを返す."
  (or (locate-dominating-file default-directory "GTAGS")
      default-directory))

(defun my/gtags--run (flag input)
  "GNU Global を FLAG と INPUT で実行し xref アイテムのリストを返す.
シェルを経由せず call-process で直接実行する。
終了コード: 0=成功, 1=一致なし（正常）, ≥2=実行エラー。"
  (let* ((root (my/gtags--project-root))
         (default-directory root)
         (xrefs '()))
    (with-temp-buffer
      (let ((exit-code (call-process "global" nil t nil
                                     "--result=grep" flag "--" input)))
        (when (>= exit-code 2)
          (user-error "global エラー (exit %d): %s"
                      exit-code (string-trim (buffer-string)))))
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at "^\\(.+\\):\\([0-9]+\\):\\(.*\\)$")
          (let ((file (expand-file-name (match-string 1) root))
                (lnum (string-to-number (match-string 2)))
                (text (string-trim (match-string 3))))
            (push (xref-make text (xref-make-file-location file lnum 0)) xrefs)))
        (forward-line 1)))
    (nreverse xrefs)))

(defun my/gtags--show (xrefs input)
  "XREFS を表示: 単一結果は直接ジャンプ、複数結果は consult-xref で表示.
呼び出し元で xref-push-marker-stack を実行済みであること。"
  (cond
   ((= (length xrefs) 1)
    (let ((loc (xref-item-location (car xrefs))))
      (find-file (xref-file-location-file loc))
      (goto-char (point-min))
      (forward-line (1- (xref-file-location-line loc)))))
   (t
    (funcall xref-show-xrefs-function
             (lambda () xrefs)
             `((window . ,(selected-window)))))))

;; ── 検索コマンド ─────────────────────────────────────────

(defun my/gtags-find-definition (&optional prompt)
  "カーソル位置のシンボルの定義を検索.
C-u 付きで呼ぶとシンボルを手動入力できる。"
  (interactive "P")
  (let* ((default (thing-at-point 'symbol t))
         (symbol (if prompt
                     (read-string
                      (if default (format "Find definition (default %s): " default)
                        "Find definition: ")
                      nil nil default)
                   (or default (read-string "Find definition: "))))
         (xrefs (my/gtags--run "-d" symbol)))
    (if xrefs
        (progn (xref-push-marker-stack)
               (my/gtags--show xrefs symbol))
      (message "見つかりません: %s" symbol))))

(defun my/gtags-find-references (&optional prompt)
  "カーソル位置のシンボルの参照を検索.
C-u 付きで呼ぶとシンボルを手動入力できる。"
  (interactive "P")
  (let* ((default (thing-at-point 'symbol t))
         (symbol (if prompt
                     (read-string
                      (if default (format "Find references (default %s): " default)
                        "Find references: ")
                      nil nil default)
                   (or default (read-string "Find references: "))))
         (xrefs (my/gtags--run "-r" symbol)))
    (if xrefs
        (progn (xref-push-marker-stack)
               (my/gtags--show xrefs symbol))
      (message "見つかりません: %s" symbol))))

(defun my/gtags-find-symbol ()
  "シンボル検索（定義・参照以外の出現箇所）."
  (interactive)
  (let* ((default (thing-at-point 'symbol t))
         (input (read-string
                 (if default (format "Find symbol (default %s): " default)
                   "Find symbol: ")
                 nil nil default))
         (xrefs (my/gtags--run "-s" input)))
    (if xrefs
        (progn (xref-push-marker-stack)
               (my/gtags--show xrefs input))
      (message "見つかりません: %s" input))))

(defun my/gtags-find-file ()
  "ファイル名パターンで検索."
  (interactive)
  (let* ((default (thing-at-point 'filename t))
         (input (read-string
                 (if default (format "Find file (default %s): " default)
                   "Find file: ")
                 nil nil default))
         (xrefs (my/gtags--run "-P" input)))
    (if xrefs
        (progn (xref-push-marker-stack)
               (my/gtags--show xrefs input))
      (message "見つかりません: %s" input))))

;; 'global -uv' を用いた GTAGS の手動フル更新
(defun update-gtags ()
  "Update GTAGS database."
  (interactive)
  (when (and (buffer-file-name) (executable-find "global"))
    (start-process "gtags-update" nil "global" "-uv")))

(provide 'my-gtags)
;;; my-gtags.el ends here
