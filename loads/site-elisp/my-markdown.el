;;; my-markdown.el --- Markdown カスタム関数群 -*- lexical-binding: t; -*-
;;; Commentary:
;; markdown-mode で使用するカスタム変数と関数

;;; Code:

(declare-function markdown-export "markdown-mode")
(declare-function markdown-export-file-name "markdown-mode")

;;; 変数定義（コンフィギュレーション）
;; Markdown 用のカスタム CSS ファイルのパス
(defvar my-markdown-css-file
  (my-set-custom "css/markdown-style.css")
  "Path to custom markdown CSS file.")

;; Markdown 用のカスタム JavaScript ファイルのパス
(defvar my-markdown-js-file
  (my-set-custom "js/markdown-script.js")
  "Path to custom markdown JS file.")

;; CSS を遅延ロードしつつ HTML に埋め込む関数
(defun my-markdown-load-css ()
  "Load the content of the markdown CSS file."
  (when (file-exists-p my-markdown-css-file)
    (with-temp-buffer
      (insert-file-contents my-markdown-css-file)
      (buffer-string))))

;; C-c TAB の制御を変更する関数
(defun my-markdown-insert-tab ()
  "C-c TAB を押したときに 4 スペースを挿入する"
  (interactive)
  (insert "    "))

;; Pandoc コマンドの設定（遅延構築: 初回参照時にサブプロセスでバージョン取得）
;; その他 option: "--toc --toc-depth=2", "--highlight-style=tango"
(defun my-get-pandoc-version ()
  "Pandoc のバージョン番号を取得し、数値で返す。"
  (let* ((version-str (shell-command-to-string "pandoc --version | head -n 1 | awk '{print $2}'"))
         (version-num (when (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)" version-str)
                        (string-to-number (match-string 1 version-str)))))
    version-num))

(defvar my-markdown-pandoc-command nil
  "Pandoc コマンド文字列。初回参照時に遅延構築される。")

(defun my-markdown-pandoc-command ()
  "Pandoc コマンドを遅延構築して返す。"
  (or my-markdown-pandoc-command
      (setq my-markdown-pandoc-command
            (let ((pandoc-ver (my-get-pandoc-version)))
              (concat "pandoc"
                      " -s"
                      " --number-sections"
                      " --toc --toc-depth=3"
                      (if (and pandoc-ver (>= pandoc-ver 3))
                          " --embed-resources --standalone"
                        " --self-contained")
                      " -f markdown -t html5"
                      " --css " (shell-quote-argument my-markdown-css-file)
                      " --include-after-body " (shell-quote-argument my-markdown-js-file))))))

(defun my-markdown-export (&optional force-pandoc)
  "現在のファイルを mdh で HTML に変換する。
未保存の変更は先に保存する。mdh がない場合や FORCE-PANDOC
（対話時は prefix 引数）が非 nil の場合は、従来の `markdown-export' を使う。
mdh の実行失敗は *mdh* バッファに表示する。"
  (interactive "P")
  (unless buffer-file-name
    (user-error "先に Markdown をファイルへ保存してください"))
  (let ((mdh (unless (or force-pandoc (file-remote-p buffer-file-name))
               (executable-find "mdh"))))
    (if (not mdh)
        (progn
          (message "従来の Markdown 変換（Pandoc）を使用します")
          (markdown-export))
      (let* ((source (expand-file-name buffer-file-name))
             (output (markdown-export-file-name ".html"))
             (output-buffer (find-buffer-visiting output))
             (log-buffer (get-buffer-create "*mdh*"))
             (default-directory (file-name-directory source)))
        (when (and output-buffer (buffer-modified-p output-buffer))
          (user-error "出力先の HTML バッファに未保存の変更があります"))
        (save-buffer)
        (with-current-buffer log-buffer
          (let ((inhibit-read-only t)) (erase-buffer)))
        ;; シェルを介さず、空白や日本語を含むパスをそのまま引数で渡す。
        (let ((status (process-file mdh nil log-buffer nil source "-o" output)))
          (unless (and (eq status 0) (file-exists-p output))
            (display-buffer log-buffer)
            (user-error "mdh の変換に失敗しました（%s）。*mdh* を確認してください" status)))
        (when output-buffer
          (with-current-buffer output-buffer (revert-buffer t t)))
        (message "mdh: %s" output)
        output))))

(defun my/markdown-setup ()
  "Things that must run *after* `markdown-mode' comes up."
  (flyspell-mode  1)                   ; スペルチェック
  (visual-line-mode 1)                 ; ソフト折り返し
  (display-line-numbers-mode 1)        ; 行番号
  (outline-minor-mode 1)               ; 見出し折りたたみ
  (electric-pair-mode 1)               ; 括弧補完
  (setq-local markdown-indent-on-enter t))

;; 折りたたみ `<details>` を挿入
(defun markdown-insert-details ()
  "Insert <details> HTML tag with a <summary>."
  (interactive)
  (insert "<details><summary>text</summary><div>\n\n</div></details>"))

(provide 'my-markdown)
;;; my-markdown.el ends here
