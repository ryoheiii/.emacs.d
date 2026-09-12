;;; my-test-gui.el --- 実 GUI の起動と補完検証 -*- lexical-binding: t; -*-
;;; Commentary:
;; make test-gui から隔離環境で実行する。Linux の X11/Xvfb と timeout が必要。
;;; Code:
(require 'ert)
(require 'my-test-startup)

(ert-deftest my-test-gui-frame-and-modeline ()
  :tags '(:gui)
  (should (display-graphic-p))
  (should (frame-visible-p (selected-frame)))
  (should (bound-and-true-p doom-modeline-mode))
  (should-not (string-empty-p (format-mode-line mode-line-format))))

(ert-deftest my-test-gui-corfu-commit ()
  :tags '(:gui)
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (corfu-mode 1)
      (setq-local completion-at-point-functions
                  (list (lambda () (list (point-min) (point-max) '("alpha" "alpine")))))
      (insert "al")
      (unwind-protect
          (progn
            (completion-at-point)
            (corfu--exhibit)
            (should (frame-live-p corfu--frame))
            (should (frame-visible-p corfu--frame))
            (corfu-next (- (cl-position "alpha" corfu--candidates :test #'equal) corfu--index))
            (corfu-insert)
            (should (equal (buffer-string) "alpha")))
        (corfu-quit)))))

(ert-deftest my-test-gui-clipboard ()
  :tags '(:gui)
  (gui-set-selection 'CLIPBOARD "日本語 clipboard fixture")
  (should (equal (gui-get-selection 'CLIPBOARD 'UTF8_STRING)
                 "日本語 clipboard fixture")))

(defun my-test-gui-run ()
  "遅延設定後の GUI を検証し、ログと終了コードを返す。"
  (let ((status 1))
    (condition-case err
        (let ((stats (ert-run-tests-batch '(tag :gui))))
          (when (and (= (ert-stats-total stats) 3)
                     (= (ert-stats-skipped stats) 0)
                     (= (ert-stats-completed-unexpected stats) 0)
                     (null (my-test-startup-check-warnings)))
            (setq status 0)))
      (error (message "GUI 検査エラー: %S" err)))
    (with-temp-file (expand-file-name "gui-results.log" user-emacs-directory)
      (insert-buffer-substring "*Messages*"))
    (kill-emacs status)))

(add-hook 'window-setup-hook
          (lambda () (run-with-idle-timer 3 nil #'my-test-gui-run)))
(provide 'my-test-gui)
;;; my-test-gui.el ends here
