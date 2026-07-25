;;; my-test-unit.el --- パスヘルパーの回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; early-init.el が定義するパスヘルパーを表駆動で検証する。

;;; Code:

(require 'ert)

;;;;; [Group] Unit - パスヘルパー ;;;;;
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

;;;;; [Group] Unit - バックアップディレクトリ ;;;;;
(ert-deftest my-test-unit-backup-directory-exists ()
  "auto-save の保存先は early-init.el が事前作成する (transforms は作成しない)."
  :tags '(:unit)
  (should (file-directory-p my-backup-dir)))

(provide 'my-test-unit)
;;; my-test-unit.el ends here
