;;; my-test-cpp-lane.el --- 文法レーンと必須テストの検証 -*- lexical-binding: t; -*-
;;; Commentary:
;; 文法の有無により排他的なテストだけを skip 可能とし、想定外 skip を検出する。
;;; Code:
(require 'ert)
(require 'cl-lib)
(defconst my-test-cpp-lane--ts-tests
  '(my-test-cpp-config-c-ts-indent-google-equivalent
    my-test-cpp-config-c-ts-error-indent
    my-test-cpp-config-c-ts-error-indent-scope
    my-test-cpp-config-c-ts-error-context-preserves-point
    my-test-cpp-config-c-ts-electric-typing))
(defconst my-test-cpp-lane--cc-tests
  '(my-test-cpp-config-google-style my-test-cpp-config-treesit-fallback))
(defun my-test-cpp-lane-run ()
  "指定レーンで ERT を実行し、不足と予期しない skip を拒否する。"
  (let* ((lane (or (getenv "TEST_TREESIT_EXPECT") "auto"))
         (ready (my/treesit-cc-grammar-ready-p 'cpp))
         (allowed (append (if ready my-test-cpp-lane--cc-tests my-test-cpp-lane--ts-tests)
                          (when (executable-find "irony-server")
                            '(my-test-cpp-config-irony-server-detection))
                          (unless (and (fboundp 'treesit-available-p) (treesit-available-p))
                            '(my-test-cpp-config-treesit-grammar-dir-isolated)))))
    (unless (member lane '("auto" "with" "without")) (error "不正な文法レーン"))
    (when (equal lane "with")
      (unless (and ready (my/treesit-cc-grammar-ready-p 'c))
        (error "with レーンには C/C++ 両文法が必要です")))
    (when (and (equal lane "without")
               (or ready (my/treesit-cc-grammar-ready-p 'c)))
      (error "without レーンに文法が見つかりました"))
    (let ((selected (mapcar #'ert-test-name (ert-select-tests '(tag :cpp-config) t))))
      (dolist (name (append my-test-cpp-lane--ts-tests my-test-cpp-lane--cc-tests))
        (unless (memq name selected)
          (error "必須テストが実行対象にありません: %s" name))))
    (let ((stats (ert-run-tests-batch '(tag :cpp-config))))
      (dolist (test (ert-select-tests '(tag :cpp-config) t))
        (when (and (ert-test-skipped-p (ert-test-most-recent-result test))
                   (not (memq (ert-test-name test) allowed)))
          (error "予期しない skip: %s" (ert-test-name test))))
      (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))))
(provide 'my-test-cpp-lane)
;;; my-test-cpp-lane.el ends here
