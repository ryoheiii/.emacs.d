;;; my-test-gui.el --- 実 GUI の起動と補完検証 -*- lexical-binding: t; -*-
;;; Commentary:
;; make test-gui から隔離環境で実行する。Linux の X11/Xvfb と timeout が必要。
;;; Code:
(require 'ert)
(require 'my-test-startup)

(defun my-test-gui--capture (name)
  "指定されている場合だけ、実画面を NAME の PNG に保存する。"
  (when-let* ((directory (getenv "MY_TEST_GUI_CAPTURE_DIR")))
    (unless (file-name-absolute-p directory)
      (error "MY_TEST_GUI_CAPTURE_DIR は絶対パスで指定してください"))
    (make-directory directory t)
    (redisplay t)
    (should (= 0 (call-process
                  "python3" nil nil nil "-c"
                  "from PIL import ImageGrab; import os, sys; ImageGrab.grab(xdisplay=os.environ['DISPLAY']).save(sys.argv[1])"
                  (expand-file-name name directory))))))

(ert-deftest my-test-gui-japanese-which-key-and-mx ()
  "GUI の候補と M-x の注釈に日本語を表示できること。"
  :tags '(:gui)
  (save-window-excursion
    (unwind-protect
        (progn
          (which-key-show-keymap 'my/key-help-map t)
          (should (get-buffer-window which-key--buffer))
          (with-current-buffer which-key--buffer
            (should (string-match-p "操作を探す" (buffer-string))))
          (my-test-gui--capture "01-which-key.png"))
      (which-key--hide-popup))
    (let (inspected)
      (condition-case nil
          (minibuffer-with-setup-hook
              (lambda ()
                (insert "save-buffer")
                (vertico--update)
                (vertico--exhibit)
                (should (string-match-p
                         "ファイルを保存"
                         (overlay-get vertico--candidates-ov 'before-string)))
                (my-test-gui--capture "02-mx.png")
                (setq inspected t)
                (setq unread-command-events (list ?\C-g)))
            (call-interactively #'execute-extended-command))
        (quit nil))
      (should inspected))))

(ert-deftest my-test-gui-japanese-help-link-and-back ()
  "キー説明のリンクと戻る操作が実際のキー入力で使えること。"
  :tags '(:gui)
  (save-window-excursion
    (with-temp-buffer
      (rename-buffer "gui-help-fixture" t)
      (switch-to-buffer (current-buffer))
      (let ((origin (current-buffer)))
        (execute-kbd-macro (kbd "C-c h k C-x C-s"))
        (select-window (get-buffer-window (help-buffer)))
        (should (string-prefix-p "日本語の説明: ファイルを保存" (buffer-string)))
        (my-test-gui--capture "03-key-help.png")
        (goto-char (point-min))
        (let ((button (next-button (point-min))))
          (while (and button (not (eq (button-get button 'type) 'help-function)))
            (setq button (next-button (button-end button))))
          (should button)
          (goto-char (button-start button)))
        (let ((before help-xref-stack-item))
          (execute-kbd-macro (kbd "RET"))
          (select-window (get-buffer-window (help-buffer)))
          (should-not (equal before help-xref-stack-item))
          (execute-kbd-macro (kbd "l"))
          (select-window (get-buffer-window (help-buffer)))
          (should (equal before help-xref-stack-item)))
        (should (string-prefix-p "日本語の説明: ファイルを保存" (buffer-string)))
        (execute-kbd-macro (kbd "q"))
        (popwin:close-popup-window-if-necessary)
        (should (eq (current-buffer) origin))))))

(ert-deftest my-test-gui-japanese-help-lists ()
  "キーとモードの一覧で日本語説明とコマンドリンクを両立すること。"
  :tags '(:gui)
  (save-window-excursion
    (with-temp-buffer
      (rename-buffer "gui-bindings-fixture" t)
      (switch-to-buffer (current-buffer))
      (let ((map (make-sparse-keymap)))
        (keymap-set map "C-c z" #'save-buffer)
        (use-local-map map))
      (execute-kbd-macro (kbd "C-c h b"))
      (select-window (get-buffer-window (help-buffer)))
      (goto-char (point-min))
      (should (re-search-forward "C-c z[ \t]+save-buffer.*ファイルを保存" nil t))
      (beginning-of-line)
      (search-forward "save-buffer")
      (should (button-at (1- (point))))
      (recenter 3)
      (my-test-gui--capture "04-bindings.png")
      (execute-kbd-macro (kbd "q"))
      ;; idle timer 内の ERT では次の idle を待てないため、既存の後処理を済ませる。
      (popwin:close-popup-window-if-necessary)
      (org-mode)
      (execute-kbd-macro (kbd "C-c h m"))
      (select-window (get-buffer-window (help-buffer)))
      (goto-char (point-min))
      (should (re-search-forward "org-todo.*タスクの状態を変更" nil t))
      (beginning-of-line)
      (search-forward "org-todo")
      (should (button-at (1- (point))))
      (recenter 3)
      (my-test-gui--capture "05-mode-help.png")
      (execute-kbd-macro (kbd "q"))
      (popwin:close-popup-window-if-necessary))))

(ert-deftest my-test-gui-japanese-magit-exit ()
  "Git の入口を閉じた後に元の編集バッファで通常入力できること。"
  :tags '(:gui)
  (require 'magit)
  (save-window-excursion
    (with-temp-buffer
      (rename-buffer "gui-git-fixture" t)
      (switch-to-buffer (current-buffer))
      (let ((origin (current-buffer)))
        (unwind-protect
            (progn
              (execute-kbd-macro (kbd "C-x M-g"))
              (with-current-buffer transient--buffer-name
                (should (get-buffer-window (current-buffer)))
                (should (string-match-p "Git の操作" (buffer-string)))
                (should (string-match-p "コミット" (buffer-string))))
              (my-test-gui--capture "06-git-menu.png")
              (execute-kbd-macro (kbd "C-g"))
              (should-not (get-buffer-window transient--buffer-name))
              (should-not transient--transient-map)
              (execute-kbd-macro (kbd "a"))
              (should (eq (current-buffer) origin))
              (should (equal (buffer-string) "a")))
          (when (get-buffer-window transient--buffer-name)
            (execute-kbd-macro (kbd "C-g"))))))))

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
  (set-frame-size (selected-frame) 100 35)
  (let ((status 1))
    (condition-case err
        (let ((stats (ert-run-tests-batch '(tag :gui))))
          (when (and (= (ert-stats-total stats) 7)
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
