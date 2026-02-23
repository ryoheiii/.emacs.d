;;; my-markdown.el --- Markdown カスタム関数群 -*- lexical-binding: t; -*-
;;; Commentary:
;; markdown-mode で使用するカスタム変数と関数

;;; Code:

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

(defun my/markdown-setup ()
  "Things that must run *after* `markdown-mode' comes up."
  (flyspell-mode  1)                   ; スペルチェック
  (visual-line-mode 1)                 ; ソフト折り返し
  (display-line-numbers-mode 1)        ; 行番号
  (outline-minor-mode 1)               ; 見出し折りたたみ
  (electric-pair-mode 1)               ; 括弧補完
  (setq-local markdown-indent-on-enter t)
  ;; 4 スペースを TAB で挿入
  (local-set-key (kbd "C-c TAB") #'my-markdown-insert-tab))

;; 折りたたみ `<details>` を挿入
(defun markdown-insert-details ()
  "Insert <details> HTML tag with a <summary>."
  (interactive)
  (insert "<details><summary>text</summary><div>\n\n</div></details>"))

(provide 'my-markdown)
;;; my-markdown.el ends here
