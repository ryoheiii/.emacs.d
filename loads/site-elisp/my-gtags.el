;;; my-gtags.el --- ggtags カスタム関数群 -*- lexical-binding: t; -*-
;;; Commentary:
;; C/C++ タグ検索を補完付き入力と結果選択に統一する。
;; eglot 管理下でカーソル位置の名前を確定した場合は xref (LSP)、
;; それ以外は GNU Global を使い、複数の結果は Consult で表示する。
;; 名前の候補一覧は DB ごとにキャッシュし、ggtags の補完 (CAPF) と共有する。

;;; Code:

(require 'xref)
(require 'subr-x)

(declare-function consult-xref "consult-xref")

(defvar my/gtags-history nil "タグ検索のシンボル名履歴。")
(defvar my/gtags-file-history nil "タグ検索のファイル名履歴。")

;;; global 直接実行エンジン

(defun my/gtags--project-root ()
  "GTAGS がある親ディレクトリを返す。見つからなければ現在のディレクトリ。"
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
               (signal err)))))
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
          (completing-read "移動先を選択: " (list label) nil t)
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

;;; 候補キャッシュ
;; 候補は `global -c' で DB の全シンボルを列挙するため、大規模プロジェクトでは
;; 1 回 0.5 秒前後かかる。種別と DB ごとに結果を保持し、タグファイルが更新される
;; まで再利用する。DB の所在は環境変数を自前で解釈せず `global -p' に任せる。

(defvar my/gtags-candidates-cache-size 2
  "候補一覧を保持する件数 (種別と DB の組ごとに 1 件)。0 ならキャッシュしない.
0 にすると、保持中の一覧も次の候補取得時に解放する。")

(defvar my/gtags--candidates-cache nil
  "候補一覧のキャッシュ。新しく使った順に並べる.
要素は ((FLAG DBDIR ENV) STAMP . CANDIDATES)。ENV は `my/gtags--env'、
STAMP は `my/gtags--db-stamp' の値。")

(defconst my/gtags--db-dirs-limit 64
  "`my/gtags--db-dirs' に記憶する件数の上限。")

(defvar my/gtags--db-dirs nil
  "((ROOT . ENV) . DBDIR) の対応表。新しく求めた順に並べる.
ROOT は `my/gtags--project-root'、ENV は `my/gtags--env' の値。
GTAGSDBPATH で DB を分けた構成では ROOT がサブディレクトリごとに変わるため、
キャッシュのキーには ROOT ではなく DBDIR を使う。")

(defun my/gtags--env ()
  "GNU Global が DB を探すときに参照する環境変数の値の組を返す.
GTAGSROOT / GTAGSDBPATH / GTAGSLIBPATH の順。"
  (mapcar #'getenv '("GTAGSROOT" "GTAGSDBPATH" "GTAGSLIBPATH")))

(defun my/gtags--libpath-p ()
  "GTAGSLIBPATH が空でなければ non-nil (候補が他の DB にも依存する)."
  (let ((libpath (getenv "GTAGSLIBPATH")))
    (and libpath (not (string-empty-p libpath)))))

(defun my/gtags--cache-limit ()
  "候補一覧を保持する件数を返す。不正な値は 0 (キャッシュしない) とみなす."
  (if (natnump my/gtags-candidates-cache-size) my/gtags-candidates-cache-size 0))

(defun my/gtags--cacheable-p ()
  "現在の環境で候補一覧をキャッシュできるなら non-nil."
  (and (> (my/gtags--cache-limit) 0)
       (not (file-remote-p default-directory))
       (not (my/gtags--libpath-p))))

(defun my/gtags--known-db-dir (root env)
  "記憶した ROOT と ENV の DB ディレクトリを返す。GTAGS が無くなっていれば nil."
  (let ((dir (cdr (assoc (cons root env) my/gtags--db-dirs))))
    (and dir (file-exists-p (expand-file-name "GTAGS" dir)) dir)))

(defun my/gtags--resolve-db-dir (root env)
  "ROOT で `global -p' を実行して DB ディレクトリを求め、ENV と組にして記憶する.
`my/gtags--call' と同じディレクトリ・環境で実行する。見つからなければ nil。"
  (let ((dir (let ((default-directory root))
               (with-temp-buffer
                 (when (eql 0 (condition-case nil
                                  (call-process "global" nil '(t nil) nil "-p")
                                (file-error nil)))
                   (let ((output (string-trim (buffer-string))))
                     (unless (string-empty-p output)
                       (file-name-as-directory (expand-file-name output)))))))))
    (setq my/gtags--db-dirs (assoc-delete-all (cons root env) my/gtags--db-dirs))
    (when (and dir (file-exists-p (expand-file-name "GTAGS" dir)))
      (push (cons (cons root env) dir) my/gtags--db-dirs)
      (setq my/gtags--db-dirs (take my/gtags--db-dirs-limit my/gtags--db-dirs))
      dir)))

(defun my/gtags--db-dir ()
  "GNU Global が現在の環境で参照する DB のディレクトリを返す.
`my/gtags--call' と同じディレクトリ・環境で `global -p' を実行して求め、
プロジェクトルートと環境変数の組ごとに記憶する (記憶した DB から GTAGS が
消えていれば求め直す)。GTAGSLIBPATH が空でない、リモート、DB が見つからない
場合は nil。"
  (unless (or (file-remote-p default-directory) (my/gtags--libpath-p))
    (let ((root (my/gtags--project-root))
          (env (my/gtags--env)))
      (or (my/gtags--known-db-dir root env)
          (my/gtags--resolve-db-dir root env)))))

(defun my/gtags--db-stamp (dbdir)
  "DBDIR のタグファイル (GTAGS / GRTAGS / GPATH) の状態を表す値を返す.
各ファイルの更新時刻・状態変更時刻・サイズ・inode の組で、無いファイルは nil。"
  (mapcar (lambda (name)
            (let ((attrs (file-attributes (expand-file-name name dbdir))))
              (and attrs
                   (list (file-attribute-modification-time attrs)
                         (file-attribute-status-change-time attrs)
                         (file-attribute-size attrs)
                         (file-attribute-inode-number attrs)))))
          '("GTAGS" "GRTAGS" "GPATH")))

(defun my/gtags--cache-trim ()
  "保持件数を `my/gtags-candidates-cache-size' 以内へ減らす (0 なら全て解放する)."
  (let ((limit (my/gtags--cache-limit)))
    (when (length> my/gtags--candidates-cache limit)
      (setq my/gtags--candidates-cache (take limit my/gtags--candidates-cache)))))

(defun my/gtags--cache-lookup (flag dbdir env)
  "FLAG・DBDIR・ENV の保持中のエントリが DBDIR の現状と一致すれば返す.
返すエントリは新しく使った順の先頭へ移す。"
  (let ((entry (assoc (list flag dbdir env) my/gtags--candidates-cache)))
    (when (and entry (equal (cadr entry) (my/gtags--db-stamp dbdir)))
      (setq my/gtags--candidates-cache
            (cons entry (delq entry my/gtags--candidates-cache)))
      entry)))

(defun my/gtags--list-candidates (flag)
  "FLAG の検索で使う名前を `global -c' で全て列挙する."
  (split-string (my/gtags--call "-c" flag "--" "") "\n" t))

(defun my/gtags--fetch-candidates (flag dbdir env)
  "FLAG の名前を列挙し、DBDIR が列挙の前後で変わらなければ ENV と組で保持する."
  (let ((key (list flag dbdir env)))
    ;; 古いエントリは列挙の成否にかかわらず捨てる。
    (setq my/gtags--candidates-cache
          (assoc-delete-all key my/gtags--candidates-cache))
    (let* ((stamp (my/gtags--db-stamp dbdir))
           (candidates (my/gtags--list-candidates flag)))
      ;; 保存時の非同期更新 (`ggtags-update-on-save') と重なった結果は保持しない。
      (when (equal stamp (my/gtags--db-stamp dbdir))
        (push (cons key (cons stamp candidates)) my/gtags--candidates-cache)
        (my/gtags--cache-trim))
      candidates)))

(defun my/gtags--candidates (flag)
  "FLAG の検索で使う名前を取得する.
空 prefix で一度だけ全候補を取得し、途中一致・複数語の絞り込みは
Emacs の補完スタイルに任せる。結果は種別と DB ごとに保持し、保持されている間は
タグファイルが変わるまで `global' を起動せずに同じリストを返す (呼び出し側で
変更しない)。保持数 `my/gtags-candidates-cache-size' を超えると最も古く使った
一覧から捨て、次に使うときに取り直す。
失敗 (`user-error') と、列挙中にタグファイルが変わった結果は保持しない。"
  (my/gtags--cache-trim)
  (if (not (my/gtags--cacheable-p))
      (my/gtags--list-candidates flag)
    (let* ((root (my/gtags--project-root))
           (env (my/gtags--env))
           (known (my/gtags--known-db-dir root env))
           ;; 記憶した DB で一致すれば `global' を起動しない。
           (entry (and known (my/gtags--cache-lookup flag known env)))
           ;; 未登録・不一致なら DB を求め直す。別のディレクトリから同じ DB を
           ;; 引いた場合は、ここで既存のエントリに一致する。
           (dbdir (unless entry (my/gtags--resolve-db-dir root env))))
      (cond (entry (cddr entry))
            ((not dbdir) (my/gtags--list-candidates flag))
            ((setq entry (my/gtags--cache-lookup flag dbdir env)) (cddr entry))
            (t (my/gtags--fetch-candidates flag dbdir env))))))

;;; ggtags 補完の差し替え
;; ggtags の補完表は直前の prefix 1 件しか覚えず、補完スタイルが空 prefix で
;; 問い合わせるたびに全件を列挙し直す。C-t d と同じ候補キャッシュを使う補完へ
;; 差し替える。ggtags はロード済みの前提で呼ばれるため、ここでは require しない。

(defvar ggtags-mode)
(defvar ggtags-bounds-of-tag-function)
(defvar ggtags-process-environment)
(declare-function ggtags-completion-at-point "ggtags")

(defun my/gtags--capf-delegate-p ()
  "ggtags 本来の補完へ任せるなら non-nil.
ggtags は `ggtags-process-environment' を展開した環境で `global' を実行するため、
これを使うプロジェクトでは C-t と異なる DB を参照しうる。キャッシュを使えない
環境と、GTAGS もその所在を示す環境変数も無い (DB が無い) 場合も任せる。"
  (or (bound-and-true-p ggtags-process-environment)
      (not (my/gtags--cacheable-p))
      (not (or (locate-dominating-file default-directory "GTAGS")
               (getenv "GTAGSROOT") (getenv "GTAGSDBPATH")))))

(defun my/gtags--completion-table ()
  "C-t の定義検索 (`-d') と候補を共有する補完テーブルを返す.
打鍵中に使うため `global' の失敗はエラーにせず候補なしとし、同じテーブルでは
再試行しない (次の補完で改めて取得する)。"
  (let ((buffer (current-buffer))
        failed)
    (completion-table-dynamic
     (lambda (_prefix)
       (unless failed
         (with-current-buffer (if (buffer-live-p buffer) buffer (current-buffer))
           (condition-case nil
               (my/gtags--candidates "-d")
             ((user-error file-error)
              (setq failed t)
              nil)))))
     t)))

(defun my/gtags-completion-at-point ()
  "`ggtags-completion-at-point' と同じ範囲を、C-t と共有する候補で補完する.
候補は `my/gtags--candidates' の定義 (`-d') のキャッシュから取り、タグファイルが
変わるまで `global' を起動しない。`my/gtags--capf-delegate-p' の場合と DB を
特定できない場合は `ggtags-completion-at-point' へそのまま任せる。"
  (if (my/gtags--capf-delegate-p)
      (ggtags-completion-at-point)
    (pcase (funcall ggtags-bounds-of-tag-function)
      (`(,beg . ,end)
       (when (< beg end)
         (if (my/gtags--db-dir)
             (list beg end (my/gtags--completion-table))
           (ggtags-completion-at-point)))))))

(defun my/gtags-use-cached-capf ()
  "ローカルの補完関数の `ggtags-completion-at-point' を差し替える.
`ggtags-mode-hook' から呼ぶ。有効化時は ggtags と同じく末尾へ
`my/gtags-completion-at-point' を置き、無効化時は取り除く。
ggtags 側の補完関数が無ければ何もしない。"
  (when (fboundp 'ggtags-completion-at-point)
    (if (bound-and-true-p ggtags-mode)
        (when (memq #'ggtags-completion-at-point completion-at-point-functions)
          (remove-hook 'completion-at-point-functions #'ggtags-completion-at-point t)
          (add-hook 'completion-at-point-functions #'my/gtags-completion-at-point t t))
      (remove-hook 'completion-at-point-functions #'my/gtags-completion-at-point t))))

;;; 検索コマンド

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
  (my/gtags--find "-d" prompt "定義を探す名前: "))

(defun my/gtags-find-references (&optional prompt)
  "名前を補完して参照を検索する.
カーソル位置の名前を初期入力にする。C-u 付きなら GNU Global を使う。"
  (interactive "P")
  (my/gtags--find "-r" prompt "使用箇所を探す名前: "))

(defun my/gtags-find-symbol ()
  "名前を補完し、定義のないシンボルの出現箇所を検索する."
  (interactive)
  (my/gtags--find "-s" nil "出現箇所を探す名前: "))

(defun my/gtags-find-file ()
  "ファイル名を補完して検索する."
  (interactive)
  (my/gtags--find "-P" nil "探すファイル名: "))

;; global -uv でタグを差分更新する（既存データベースが対象）。
(defun update-gtags ()
  "GNU Global のタグデータベースを手動で更新する。"
  (interactive)
  (when (and (buffer-file-name) (executable-find "global"))
    (start-process "gtags-update" nil "global" "-uv")))

(provide 'my-gtags)
;;; my-gtags.el ends here
