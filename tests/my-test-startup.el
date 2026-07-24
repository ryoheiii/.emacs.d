;;; my-test-startup.el --- 起動回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; init-loader が記録したエラーを検出し、バッチ起動を fail-closed にする。

;;; Code:

(require 'subr-x)

;;;;; [Group] Startup - init-loader エラー検査 ;;;;;
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

(provide 'my-test-startup)
;;; my-test-startup.el ends here
