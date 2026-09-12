;;; my-lint-el.el --- 全管理対象の構文と設定のコンパイルを検査 -*- lexical-binding: t; -*-
;;; Commentary:
;; early-init/init 読み込み後に使う。出力先は Makefile が一時領域へ設定する。
;;; Code:
(require 'bytecomp)
(defun my-lint-el-run ()
  "引数の全 Elisp の構文と、設定ファイルの未知警告を検査する。"
  (let ((files command-line-args-left)
        (byte-compile-error-on-warn t)
        failed)
    (setq command-line-args-left nil)
    (dolist (file files)
      (condition-case err
          (progn
            (with-temp-buffer
              (insert-file-contents file)
              (emacs-lisp-mode)
              (check-parens)
              (goto-char (point-min))
              (while (progn (forward-comment (point-max)) (not (eobp)))
                (read (current-buffer))))
            (when (or (member file '("early-init.el" "init.el"))
                      (string-prefix-p "loads/inits/" file)
                      (string-prefix-p "loads/site-elisp/" file))
              (unless (byte-compile-file file) (setq failed t))))
        (error (message "%s: %s" file (error-message-string err)) (setq failed t))))
    (kill-emacs (if failed 1 0))))
(provide 'my-lint-el)
;;; my-lint-el.el ends here
