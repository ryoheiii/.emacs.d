;;; my-test-unit.el --- パスヘルパーの回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; 保存先の指定と、旧配置からの移行でデータが失われないことを検証する。

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
    (my-set-history "var/state/" ("single.el") "var/state/single.el"
                    ("nested/" "multiple.el") "var/state/nested/multiple.el")
    (my-set-backup "var/backup/" ("single.el") "var/backup/single.el"
                   ("nested/" "multiple.el") "var/backup/nested/multiple.el")
    (my-set-cache "var/cache/" ("one") "var/cache/one"
                  ("nested/" "two") "var/cache/nested/two")
    (my-set-temp "var/tmp/" ("one") "var/tmp/one"
                 ("nested/" "two") "var/tmp/nested/two")
    (my-set-db "var/state/database/" ("single.el") "var/state/database/single.el"
               ("nested/" "multiple.el") "var/state/database/nested/multiple.el")
    (my-set-straight "loads/straight/" ("single.el") "loads/straight/single.el"
                     ("nested/" "multiple.el") "loads/straight/nested/multiple.el")
    (my-set-package "var/packages/" ("single.el") "var/packages/single.el"
                    ("nested/" "multiple.el") "var/packages/nested/multiple.el"))
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

;;; 保存先の移行とデータ保全
(defun my-test-unit--write (root name content)
  "ROOT の NAME へ検査用の CONTENT を書く。"
  (let ((path (expand-file-name name root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content))))

(defun my-test-unit--read (root name)
  "ROOT の NAME の内容を返す。"
  (with-temp-buffer
    (insert-file-contents (expand-file-name name root))
    (buffer-string)))

(ert-deftest my-test-unit-runtime-migration-preserves-data ()
  "旧配置の履歴と導入物を分けて引き継ぎ、再実行しても内容を変えない。"
  :tags '(:unit)
  (let ((root (make-temp-file "my-storage-" t)))
    (unwind-protect
        (progn
          (dolist (entry '(("var/hist/history" . "history")
                           ("var/hist/irony/bin/server" . "server")
                           ("var/package/tree-sitter/c.so" . "grammar")
                           ("var/package/eln-cache/a.eln" . "compiled")
                           ("var/database/data" . "database")
                           ("var/backup/file" . "backup")
                           ("transient/values.el" . "values")
                           ("var/hist/transient/history.el" . "transient history")
                           ("projects" . "old projects")
                           ("projects.eld" . "projects")
                           ("url/cookies" . "cookies")
                           ("url/cache/page" . "cached page")
                           ("copilot-chat/chat.md" . "conversation")
                           (".cache/copilot/server" . "copilot")
                           ("eln-cache/old.eln" . "old compiled")
                           ("request/cookies" . "request cookies")
                           (".bench/now/result" . "benchmark")))
            (my-test-unit--write root (car entry) (cdr entry)))
          (my/runtime-migrate-layout root)
          (my/runtime-migrate-layout root)
          (dolist (entry '(("var/state/history" . "history")
                           ("var/packages/irony/bin/server" . "server")
                           ("var/packages/tree-sitter/c.so" . "grammar")
                           ("var/cache/eln-cache/a.eln" . "compiled")
                           ("var/state/database/data" . "database")
                           ("var/backup/file" . "backup")
                           ("var/state/transient/values.el" . "values")
                           ("var/state/transient/history.el" . "transient history")
                           ("var/state/projects" . "old projects")
                           ("var/state/projects.eld" . "projects")
                           ("var/state/url/cookies" . "cookies")
                           ("var/cache/url/page" . "cached page")
                           ("var/state/copilot-chat/chat.md" . "conversation")
                           ("var/packages/copilot/server" . "copilot")
                           ("var/cache/legacy-eln-cache/old.eln" . "old compiled")
                           ("var/state/request/cookies" . "request cookies")
                           ("var/cache/bench/now/result" . "benchmark")))
            (should (equal (my-test-unit--read root (car entry)) (cdr entry))))
          (dolist (name '("var/hist" "var/package" ".cache" "eln-cache" ".bench"
                          "var/state/url/cache"))
            (should-not (file-exists-p (expand-file-name name root)))))
      (delete-directory root t))))

(ert-deftest my-test-unit-runtime-migration-refuses-overwrite ()
  "移行先に同名ファイルがあれば両方を残す。衝突を解消すれば再開できる。"
  :tags '(:unit)
  (let ((root (make-temp-file "my-storage-conflict-" t)))
    (unwind-protect
        (progn
          (my-test-unit--write root "var/hist/history" "old")
          (my-test-unit--write root "var/state/history" "new")
          (should-error (my/runtime-migrate-layout root))
          (should (equal (my-test-unit--read root "var/hist/history") "old"))
          (should (equal (my-test-unit--read root "var/state/history") "new"))
          (rename-file (expand-file-name "var/state/history" root)
                       (expand-file-name "var/state/history.saved" root))
          (my/runtime-migrate-layout root)
          (should (equal (my-test-unit--read root "var/state/history") "old")))
      (delete-directory root t))))

(ert-deftest my-test-unit-runtime-migration-refuses-linked-entry ()
  "移行対象のリンクを勝手に付け替えたり、リンク先の内容を変更しない。"
  :tags '(:unit)
  (skip-unless (not (eq system-type 'windows-nt)))
  (let ((root (make-temp-file "my-storage-link-" t)))
    (unwind-protect
        (progn
          (my-test-unit--write root "external/history" "keep")
          (make-directory (expand-file-name "var" root))
          (make-symbolic-link "../external" (expand-file-name "var/hist" root))
          (should-error (my/runtime-migrate-layout root))
          (should (file-symlink-p (expand-file-name "var/hist" root)))
          (should (equal (my-test-unit--read root "external/history") "keep")))
      (delete-directory root t))))

(ert-deftest my-test-unit-runtime-preserves-unrelated-cache ()
  "未知のキャッシュと .cache のリンク先は移動・削除しない。"
  :tags '(:unit)
  (skip-unless (not (eq system-type 'windows-nt)))
  (let ((root (make-temp-file "my-storage-cache-" t)))
    (unwind-protect
        (progn
          (my-test-unit--write root ".cache/unknown/data" "keep")
          (my/runtime-migrate-layout root)
          (should (equal (my-test-unit--read root ".cache/unknown/data") "keep"))
          (rename-file (expand-file-name ".cache" root) (expand-file-name "external" root))
          (my-test-unit--write root "external/copilot/server" "server")
          (make-symbolic-link "external" (expand-file-name ".cache" root))
          (my/runtime-migrate-layout root)
          (should (file-symlink-p (expand-file-name ".cache" root)))
          (should (equal (my-test-unit--read root "external/copilot/server") "server")))
      (delete-directory root t))))

(ert-deftest my-test-unit-runtime-default-destinations ()
  "初回ロード前から、標準機能の保存先と一時領域を var 配下へ指定する。"
  :tags '(:unit)
  (dolist (entry '((project-list-file . "var/state/projects.eld")
                   (url-configuration-directory . "var/state/url/")
                   (url-cache-directory . "var/cache/url/")
                   (transient-levels-file . "var/state/transient/levels.el")
                   (transient-values-file . "var/state/transient/values.el")
                   (transient-history-file . "var/state/transient/history.el")
                   (temporary-file-directory . "var/tmp/")))
    (should (equal (symbol-value (car entry))
                   (expand-file-name (cdr entry) user-emacs-directory))))
  (let ((file (make-temp-file "my-storage-probe-")))
    (unwind-protect
        (should (file-in-directory-p file (expand-file-name "var/tmp" user-emacs-directory)))
      (delete-file file))))

(provide 'my-test-unit)
;;; my-test-unit.el ends here
