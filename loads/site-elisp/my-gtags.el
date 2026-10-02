;;; my-gtags.el --- ggtags カスタム関数群 -*- lexical-binding: t; -*-
;;; Commentary:
;; C/C++ タグ検索を補完付き入力と結果選択に統一する。
;; eglot 管理下でカーソル位置の名前を確定した場合は xref (LSP)、
;; それ以外は GNU Global を使い、複数の結果は Consult で表示する。

;;; Code:

(require 'xref)
(require 'subr-x)

(declare-function consult-xref "consult-xref")

(defvar my/gtags-history nil "タグ検索のシンボル名履歴。")
(defvar my/gtags-file-history nil "タグ検索のファイル名履歴。")

;; ── global 直接実行エンジン ──────────────────────────────

(defun my/gtags--project-root ()
  "GTAGS ファイルを探索してプロジェクトルートを返す."
  (or (locate-dominating-file default-directory "GTAGS")
      default-directory))

(defun my/gtags--call (&rest args)
  "GNU Global を ARGS で直接実行し、出力を返す.
終了コード 0 と 1（一致なし）以外はエラーとして通知する。"
  (let ((default-directory (my/gtags--project-root)))
    (with-temp-buffer
      (let ((status (condition-case err
                        (apply #'call-process "global" nil t nil args)
                      (file-missing (user-error "%s" (error-message-string err))))))
        (unless (and (integerp status) (memq status '(0 1)))
          (user-error "global エラー (exit %s): %s"
                      status (string-trim (buffer-string)))))
      (buffer-string))))

(defun my/gtags--run (flag input)
  "GNU Global の FLAG と INPUT から xref アイテムのリストを返す."
  (let* ((root (my/gtags--project-root))
         (default-directory root)
         (xrefs '()))
    (with-temp-buffer
      (insert (my/gtags--call "--result=grep" "--encode-path=:%" flag "--" input))
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at "^\\([^:]+\\):\\([0-9]+\\):\\(.*\\)$")
          (let ((file (expand-file-name
                       (replace-regexp-in-string
                        "%[[:xdigit:]][[:xdigit:]]"
                        (lambda (encoded)
                          (pcase (downcase encoded)
                            ("%3a" ":") ("%25" "%") (_ encoded)))
                        (match-string 1) t t)
                       root))
                (lnum (string-to-number (match-string 2)))
                (text (string-trim (match-string 3))))
            (push (xref-make text (xref-make-file-location file lnum 0)) xrefs)))
        (forward-line 1)))
    (nreverse xrefs)))

(defun my/gtags--candidates (flag)
  "FLAG の検索で使う名前を取得する.
空 prefix で一度だけ全候補を取得し、途中一致・複数語の絞り込みは
Emacs の補完スタイルに任せる。"
  (split-string (my/gtags--call "-c" flag "--" "") "\n" t))

(defun my/gtags--read (label flag default lsp-p)
  "LABEL を表示し、FLAG の候補から名前を読む.
DEFAULT は編集可能な初期入力。LSP-P が non-nil なら Global の不備で
カーソル位置の LSP 検索を妨げない。"
  (let* ((candidates
          (condition-case err
              (if (and lsp-p
                       (or (not (executable-find "global"))
                           (not (or (locate-dominating-file default-directory "GTAGS")
                                    (getenv "GTAGSROOT") (getenv "GTAGSDBPATH")))))
                  (list default)
                (my/gtags--candidates flag))
            ((user-error file-error)
             (if lsp-p
                 (progn
                   (message "Global の補完を利用できません: %s"
                            (error-message-string err))
                   (list default))
               (signal (car err) (cdr err))))))
         ;; 古い GTAGS に元の名前が無くても、RET で別名を選んでしまわない。
         (candidates (if (and lsp-p (not (member default candidates)))
                         (cons default candidates)
                       candidates))
         (input (completing-read label candidates nil nil default
                                 (if (equal flag "-P")
                                     'my/gtags-file-history
                                   'my/gtags-history)
                                 default)))
    (when (string-blank-p input)
      (user-error "検索する名前を入力してください"))
    input))

(defun my/gtags--show (fetcher &optional alist)
  "FETCHER の結果を、1 件でも明示選択してから表示する.
ALIST は xref の表示設定。複数件は Consult のプレビューを使う。"
  (let ((xrefs (funcall fetcher)))
    (if (= (length xrefs) 1)
        (let* ((item (car xrefs))
               (loc (xref-item-location item))
               (label (format "%s:%s: %s" (xref-location-group loc)
                              (or (xref-location-line loc) 0)
                              (xref-item-summary item))))
          ;; consult-xref 自身も 1 件なら入力を省略するため、ここで選択する。
          (completing-read "Go to xref: " (list label) nil t)
          (xref-pop-to-location item (alist-get 'display-action alist)))
      (consult-xref (lambda () xrefs) alist))))

(defun my/gtags--lsp-p ()
  "現在のバッファが eglot 管理下なら non-nil を返す."
  (and (fboundp 'eglot-managed-p) (eglot-managed-p)))

(defun my/gtags--find-via-global (flag input)
  "GNU Global の FLAG で INPUT を検索して結果を表示する."
  (let ((xrefs (my/gtags--run flag input)))
    (if xrefs
        (let ((xref-show-xrefs-function #'my/gtags--show))
          (xref-show-xrefs (lambda () xrefs) nil))
      (message "見つかりません: %s" input))))

;; ── 検索コマンド ─────────────────────────────────────────

(defun my/gtags--find (flag prompt label)
  "LABEL の入力欄で名前を補完し、FLAG の検索結果から選ぶ.
PROMPT が non-nil なら GNU Global を使う。"
  (let* ((default (thing-at-point (if (equal flag "-P") 'filename 'symbol) t))
         (lsp-p (and (member flag '("-d" "-r"))
                     (not prompt) default (my/gtags--lsp-p)))
         (input (my/gtags--read label flag default lsp-p)))
    (if (and lsp-p (equal input default))
        (let ((xref-show-definitions-function #'my/gtags--show)
              (xref-show-xrefs-function #'my/gtags--show))
          (condition-case nil
              (funcall (if (equal flag "-d")
                           #'xref-find-definitions #'xref-find-references)
                       input)
            (user-error (my/gtags--find-via-global flag input))))
      (my/gtags--find-via-global flag input))))

(defun my/gtags-find-definition (&optional prompt)
  "名前を補完して定義を検索する.
カーソル位置の名前を初期入力にする。C-u 付きなら GNU Global を使う。"
  (interactive "P")
  (my/gtags--find "-d" prompt "Find definition: "))

(defun my/gtags-find-references (&optional prompt)
  "名前を補完して参照を検索する.
カーソル位置の名前を初期入力にする。C-u 付きなら GNU Global を使う。"
  (interactive "P")
  (my/gtags--find "-r" prompt "Find references: "))

(defun my/gtags-find-symbol ()
  "名前を補完し、定義のないシンボルの出現箇所を検索する."
  (interactive)
  (my/gtags--find "-s" nil "Find symbol: "))

(defun my/gtags-find-file ()
  "ファイル名を補完して検索する."
  (interactive)
  (my/gtags--find "-P" nil "Find file: "))

;; 'global -uv' を用いた GTAGS の手動フル更新
(defun update-gtags ()
  "Update GTAGS database."
  (interactive)
  (when (and (buffer-file-name) (executable-find "global"))
    (start-process "gtags-update" nil "global" "-uv")))

(provide 'my-gtags)
;;; my-gtags.el ends here
