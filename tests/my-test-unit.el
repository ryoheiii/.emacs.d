;;; my-test-unit.el --- パスヘルパーの回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; early-init.el が定義するパスヘルパーを表駆動で検証する。

;;; Code:

(require 'ert)

;;; パスヘルパー
(defconst my-test-unit--path-cases
  '((my-set-emacs "" ("single.el") "single.el"
                  ("nested/" "multiple.el") "nested/multiple.el")
    (my-set-loads "loads/" ("single.el") "loads/single.el"
                  ("nested/" "multiple.el") "loads/nested/multiple.el")
    (my-set-custom "custom/" ("single.el") "custom/single.el"
                   ("nested/" "multiple.el") "custom/nested/multiple.el")
    (my-set-history "var/hist/" ("single.el") "var/hist/single.el"
                    ("nested/" "multiple.el") "var/hist/nested/multiple.el")
    (my-set-backup "var/backup/" ("single.el") "var/backup/single.el"
                   ("nested/" "multiple.el") "var/backup/nested/multiple.el")
    (my-set-db "var/database/" ("single.el") "var/database/single.el"
               ("nested/" "multiple.el") "var/database/nested/multiple.el")
    (my-set-straight "loads/straight/" ("single.el") "loads/straight/single.el"
                     ("nested/" "multiple.el") "loads/straight/nested/multiple.el")
    (my-set-package "var/package/" ("single.el") "var/package/single.el"
                    ("nested/" "multiple.el") "var/package/nested/multiple.el"))
  "パスヘルパー、基底パス、引数、期待相対パスの一覧。")

(ert-deftest my-test-unit-path-helpers ()
  :tags '(:unit)
  (dolist (test-case my-test-unit--path-cases)
    (pcase-let ((`(,helper ,base ,single-args ,single-path
                           ,multiple-args ,multiple-path)
                 test-case))
      (should (equal (apply helper single-args)
                     (expand-file-name single-path user-emacs-directory)))
      (should (equal (apply helper multiple-args)
                     (expand-file-name multiple-path user-emacs-directory)))
      (should (equal (funcall helper)
                     (directory-file-name
                      (expand-file-name base user-emacs-directory)))))))

;;; バックアップディレクトリ
(ert-deftest my-test-unit-backup-directory-exists ()
  "auto-save の保存先は early-init.el が事前作成する (transforms は作成しない)."
  :tags '(:unit)
  (should (file-directory-p my-backup-dir)))

;;; load-path キャッシュ
;; load-path-filter-cache-directory-files は regexp-opt を呼ぶ。regexp-opt を preload
;; するのは X 対応ビルドだけで、端末専用ビルド (CI の Emacs など) では autoload の load が
;; フィルタを再び呼び無限再帰になる。手元の GUI 対応ビルドでは再現しないため、
;; regexp-opt を autoload へ戻した子プロセスで early-init.el の該当箇所までを評価する。
(ert-deftest my-test-unit-load-path-filter-without-preloaded-regexp-opt ()
  :tags '(:unit)
  (let* ((early-init (expand-file-name "early-init.el" user-emacs-directory))
         (driver
          `(progn
             (setq native-comp-jit-compilation nil
                   user-emacs-directory ,user-emacs-directory)
             (setq features (delq 'regexp-opt features))
             (dolist (fn '(regexp-opt regexp-opt-depth regexp-opt-charset))
               (fset fn (list 'autoload "regexp-opt")))
             (with-temp-buffer
               (insert-file-contents ,early-init)
               (let (form done)
                 (while (and (not done)
                             (setq form (ignore-errors (read (current-buffer)))))
                   (eval form t)
                   (setq done (and (eq (car-safe form) 'setq)
                                   (eq (nth 1 form) 'load-path-filter-function))))
                 (unless done (kill-emacs 3))))
             (require 'bytecomp)
             (kill-emacs (if (and load-path-filter-function (featurep 'bytecomp)) 0 4))))
         (status (call-process (expand-file-name invocation-name invocation-directory)
                               nil nil nil "-Q" "--batch"
                               "--eval" (prin1-to-string driver))))
    (should (eql status 0))))

(provide 'my-test-unit)
;;; my-test-unit.el ends here
