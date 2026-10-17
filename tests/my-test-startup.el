;;; my-test-startup.el --- 起動回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; init-loader が記録したエラーと起動時警告を検出し、
;; エラーや警告がある起動を成功扱いしない。

;;; Code:

(require 'seq)
(require 'subr-x)

;;;;; [Group] init-loader エラー検査 ;;;;;
(defun my-test-startup--format-error (entry)
  "init-loader の ENTRY をファイル名とエラー要約だけに整形する。"
  (if (string-match "\\`\\(.+\\.elc?\\)\\. \\(.*\\)\\'" entry)
      (format "%s: %s"
              (file-name-nondirectory (match-string 1 entry))
              (match-string 2 entry))
    "詳細を安全に整形できない init-loader エラー"))

(unless (fboundp 'init-loader-error-log)
  (error "init-loader-error-log が定義されていません"))

(let ((error-log (init-loader-error-log)))
  (if (string-empty-p error-log)
      (message "起動回帰テスト: init-loader エラーなし")
    (dolist (entry (string-split error-log "\n" t))
      (message "起動設定エラー: %s" (my-test-startup--format-error entry)))
    (kill-emacs 1)))

;;;;; [Group] 起動時警告検査 ;;;;;
;; 警告の記録処理は EMACS_TEST_OPTIONS / tty 用の起動ラッパーが設定する。
;; 記録処理が欠けた状態を「警告なし」と誤判定しないよう、存在も検査する。
(unless (and (boundp 'my-test--recorded-warnings)
             (advice-member-p 'my-test--record-warning 'display-warning))
  (message "起動回帰テスト: 警告レコーダーが未導入です(ハーネスの注入を確認)")
  (kill-emacs 1))

;; 許容項目を追加する場合は、テスト環境固有で警告を除去できない理由を直前に記載する。
(defconst my-test-startup--warning-allowlist
  nil
  "起動時に許容する警告の (TYPE . MESSAGE-REGEXP) リスト。")

(defun my-test-startup--warning-allowed-p (warning)
  "WARNING が起動時警告の許可リストに一致する場合は non-nil を返す。"
  (let ((type (nth 0 warning))
        (warning-message (format "%s" (nth 1 warning))))
    (seq-some
     (lambda (entry)
       (and (equal type (car entry))
            (string-match-p (cdr entry) warning-message)))
     my-test-startup--warning-allowlist)))

(defun my-test-startup-check-warnings ()
  "記録済みの起動時警告から許可されていない警告を返す。"
  (when (boundp 'my-test--recorded-warnings)
    (seq-remove #'my-test-startup--warning-allowed-p
                my-test--recorded-warnings)))

(let ((warnings (my-test-startup-check-warnings)))
  (if warnings
      (progn
        (dolist (warning warnings)
          (message "起動時警告: type=%S level=%S message=%s"
                   (nth 0 warning)
                   (nth 2 warning)
                   (format "%s" (nth 1 warning))))
        (kill-emacs 1))
    (message "起動回帰テスト: 未知の起動時警告なし")))

(provide 'my-test-startup)
;;; my-test-startup.el ends here
