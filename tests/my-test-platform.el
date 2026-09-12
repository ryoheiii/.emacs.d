;;; my-test-platform.el --- 実 OS の起動・パス検証 -*- lexical-binding: t; -*-
;;; Commentary:
;; early-init/init と起動警告検査を通した、隔離環境で実行する。
;;; Code:
(require 'ert)

(message "環境: OS=%s Emacs=%s native-comp=%S GUI=%S"
         system-type emacs-version
         (and (fboundp 'native-comp-available-p) (native-comp-available-p))
         (display-graphic-p))

(ert-deftest my-test-platform-paths ()
  :tags '(:platform)
  (dolist (path (list (my-set-history "fixture") (my-set-backup "fixture")
                     (my-set-package "fixture")))
    (should (file-name-absolute-p path))
    (should (file-in-directory-p path user-emacs-directory))))

(ert-deftest my-test-platform-input-method-guard ()
  :tags '(:platform)
  (if IS-WINDOWS
      (unless (display-graphic-p)
        (should-not (featurep 'tr-ime))
        (should-not (equal default-input-method "japanese-mozc")))
    (should (equal default-input-method "japanese-mozc"))
    (should-not (featurep 'tr-ime))))

(ert-deftest my-test-platform-node-path-separator ()
  :tags '(:platform)
  (let ((process-environment (copy-sequence process-environment))
        (exec-path (copy-sequence exec-path))
        (dir (make-temp-file "my-test-node-" t)))
    (unwind-protect
        (dolist (path-separator (delete-dups (list path-separator ";")))
          (setenv "PATH" (string-join '("first" "second") path-separator))
          (my/copilot--prepend-to-path dir)
          (my/copilot--prepend-to-path dir)
          (should (equal (split-string (getenv "PATH") path-separator t)
                         (list dir "first" "second"))))
      (delete-directory dir))))

(provide 'my-test-platform)
;;; my-test-platform.el ends here
