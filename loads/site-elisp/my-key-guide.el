;;; my-key-guide.el --- 日本語で操作とキーを探す入口 -*- lexical-binding: t; -*-
;;; Commentary:
;; 既存の補完 UI と標準ヘルプを使い、呼び出し元の有効なキーを案内する。
;; 日本語入力を切り替えなくても、英語のコマンド名で絞り込める。

;;; Code:

(require 'seq)
(require 'help-mode)
(require 'my-key-guide-ja)

(declare-function marginalia-annotate-binding "marginalia" (candidate))
(declare-function marginalia-annotate-command "marginalia" (candidate))
(declare-function marginalia-annotate-function "marginalia" (candidate))

(defvar my/key-guide-commands
  '(("ファイルを開く" . find-file)
    ("ファイルを保存" . save-buffer)
    ("別名で保存" . write-file)
    ("最近のファイル" . consult-recent-file)
    ("バッファを切り替える" . consult-buffer)
    ("バッファを閉じる" . kill-current-buffer)
    ("本文を検索" . consult-line)
    ("開いている複数の本文を検索" . my/consult-line-multi)
    ("プロジェクトのファイル内容を検索" . my/consult-ripgrep-or-grep)
    ("確認しながら置換" . query-replace)
    ("行頭へ移動" . move-beginning-of-line)
    ("行末へ移動" . move-end-of-line)
    ("選択範囲の開始位置を設定" . set-mark-command)
    ("選択範囲をコピー" . kill-ring-save)
    ("選択範囲を切り取り" . kill-region)
    ("貼り付け" . yank)
    ("コピー履歴から貼り付け" . consult-yank-pop)
    ("変更を元に戻す" . undo)
    ("変更履歴をツリー表示" . vundo)
    ("他のウィンドウへ移動・分割" . my/other-window-or-split)
    ("ウィンドウを左右に分割" . split-window-right)
    ("ウィンドウを上下に分割" . split-window-below)
    ("今のウィンドウだけ表示" . delete-other-windows)
    ("日本語入力を切り替え" . toggle-input-method)
    ("操作ガイドを読む" . my/key-guide)
    ("日本語で操作を探す" . my/command-palette)
    ("押したキーの意味を調べる" . describe-key)
    ("今使えるキーをすべて表示" . describe-bindings)
    ("今のモードの使い方" . describe-mode)
    ("コマンドの説明を読む" . describe-function)
    ("標準チュートリアルで練習" . help-with-tutorial))
  "操作検索とガイドに載せる (日本語の説明 . コマンド)。")

(defconst my/key-guide--cpp-commands
  '(("定義を探す" . my/gtags-find-definition)
    ("使用箇所を探す" . my/gtags-find-references)
    ("移動前の場所へ戻る" . xref-go-back)
    ("移動先へ進む" . xref-go-forward))
  "C/C++ バッファで追加する操作。")

(defun my/key-guide-description (command)
  "COMMAND の日本語の説明を返す。未登録の操作なら nil。"
  (or (car (rassq command my/key-guide-commands))
      (car (rassq command my/key-guide--cpp-commands))
      (car (rassq command my/key-guide--extra-descriptions))))

(defun my/key-guide-annotate-command (candidate)
  "M-x の CANDIDATE に、キーと日本語の説明を添える。"
  (if-let* ((description (my/key-guide-description (intern-soft candidate))))
      (concat (marginalia-annotate-binding candidate)
              " " (propertize description 'face 'marginalia-documentation))
    (marginalia-annotate-command candidate)))

(defun my/key-guide-annotate-function (candidate)
  "関数検索の CANDIDATE に日本語の説明を添える。"
  (if (my/key-guide-description (intern-soft candidate))
      (my/key-guide-annotate-command candidate)
    (marginalia-annotate-function candidate)))

(defun my/key-guide-annotate-key-list ()
  "標準ヘルプのキー一覧に、日本語の説明を追記する。"
  (when (and (derived-mode-p 'help-mode)
             (memq (car help-xref-stack-item) '(describe-bindings describe-mode)))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        ;; 標準の一覧にある「キー、空白、コマンド名」だけを対象にする。
        ;; 名前とリンクを残し、未登録の操作や既に説明を付けた行は変更しない。
        (while (re-search-forward "^[^\n]+[ \t]+\\([^ \t\n]+\\)$" nil t)
          (when-let* ((command (intern-soft (match-string-no-properties 1)))
                      (description (my/key-guide-description command)))
            (insert "  — " description)))))))

(defun my/key-guide-help-summary (command)
  "標準の COMMAND ヘルプの先頭に日本語の要約を添える。"
  (when-let* ((description (my/key-guide-description command)))
    ;; 標準フックは原文の挿入後に呼ばれる。原文やリンクを残して冒頭へ補足する。
    (save-excursion
      (goto-char (point-min))
      (insert "日本語の説明: " description "\n\n原文・詳細:\n\n"))))

(defun my/key-guide--binding (command)
  "今のバッファで COMMAND に届くキー、または M-x の案内を返す。"
  (let ((key (seq-find
              (lambda (key)
                ;; open/copy 等の特殊キーと、DEL へ変換される C-h は案内しない。
                (and (seq-every-p #'integerp key)
                     (not (seq-contains-p key ?\C-h))
                     (eq (key-binding key) command)))
              (where-is-internal command))))
    (if key (key-description key) (format "M-x %s" command))))

(defun my/key-guide--candidates ()
  "現在のモードとキーのリマップを反映した操作候補を返す。"
  (let ((entries (append my/key-guide-commands
                         (when (derived-mode-p 'c-mode 'c++-mode
                                               'c-ts-mode 'c++-ts-mode)
                           my/key-guide--cpp-commands))))
    (delq nil
          (mapcar
           (lambda (entry)
             (let ((command (or (command-remapping (cdr entry)) (cdr entry))))
               (when (commandp command)
                 (cons (format "%s  [%s]  %s"
                               (car entry) (my/key-guide--binding command) command)
                       command))))
           entries))))

(defvar my/command-palette-history nil
  "操作検索の入力履歴。")

;;;###autoload
(defun my/command-palette ()
  "日本語の目的または英語のコマンド名で操作を選び、実行する。
候補の角括弧には現在のバッファで有効なキーを表示する。
選択範囲と前置引数は、呼び出し元から選んだコマンドへ引き継ぐ。"
  (interactive)
  (let* ((prefix current-prefix-arg)
         (candidates (my/key-guide--candidates))
         (choice (completing-read "操作を探す（C-g で中止）: "
                                  candidates nil t nil
                                  'my/command-palette-history))
         (command (cdr (assoc choice candidates))))
    (when command
      (let ((current-prefix-arg prefix))
        ;; コマンドループに実行先を知らせ、undo 等の連続操作も保つ。
        (setq this-command command)
        (call-interactively command)))))

;;;###autoload
(defun my/key-guide ()
  "呼び出し元のバッファに合わせた日本語の操作ガイドを開く。"
  (interactive)
  ;; with-help-window の中ではヘルプ側のキーになるため、先に取得する。
  (let ((origin (buffer-name))
        (candidates (my/key-guide--candidates))
        ;; 狭い端末でも読めるよう同じウィンドウへ表示する。
        ;; popwin の help-mode 処理を経由せず、q で元のバッファへ戻す。
        (display-buffer-overriding-action '(display-buffer-same-window))
        (help-window-select t))
    (with-help-window "*操作ガイド*"
      (princ (format "操作ガイド — %s\n\n" origin))
      (princ "まず覚えるのは C-c h（操作ヘルプ）と C-g（中止）。\n")
      (princ "C- は Ctrl と同時押し。M- は Alt、または ESC を離して次のキー。\n")
      (princ "C-c h s は Ctrl+c、離して h、次に s。RET は Enter。\n\n")
      (princ "■ 操作しながら探す\n")
      (princ "  C-c h s : 日本語の目的や英語名で検索 → C-n/C-p で選択 → RET\n")
      (princ "              例: 保存、検索、save-buffer\n")
      (princ "  C-x / C-c / C-z の後で少し待つ : which-key が続きの候補を表示\n")
      (princ "  C-c h k : 調べたいキーを押すと説明（その操作は実行しない）\n")
      (princ "  C-c h b : 今使えるキーの全一覧。小さい画面で候補が省略されたときにも\n")
      (princ "  C-c h m : 今のモードの説明とキー\n")
      (princ "  M-x     : 全コマンドを英語名で検索。実行後にもキーを案内\n\n")
      (princ "■ この設定での注意\n")
      (princ "  C-h は削除キー。一般的な説明の C-h k は、ここでは C-c h k。\n")
      (princ "  C-c h t で標準チュートリアル。C-h や C-s 等は標準と異なる。\n")
      (princ "  入力途中は C-g で中止。この画面は q で元の作業へ戻る。\n\n")
      (princ "■ 元のバッファで使える代表操作（[キー] とコマンド名）\n\n")
      (dolist (entry candidates)
        (princ (car entry))
        (princ "\n")))))

(provide 'my-key-guide)
;;; my-key-guide.el ends here
