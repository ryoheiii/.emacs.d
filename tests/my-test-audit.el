;;; my-test-audit.el --- 監査で発見した操作境界の回帰検証 -*- lexical-binding: t; -*-
;;; Commentary:
;; 各コマンドを固定入力で実行し、結果と対象外データの保全を確認する。
;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'symbol-overlay)
(require 'my-gtags)

(ert-deftest my-test-audit-gnus-theme-load ()
  :tags '(:audit)
  ;; 遅延ロード時にテーマと標準 face の継承が循環すると Emacs 31 は失敗する。
  (should (require 'gnus)))

(ert-deftest my-test-audit-rename-visible-boundaries ()
  :tags '(:audit)
  (dolist (names '(("foo" "longer_name") ("count" "x") ("foo" "x\\&%s")))
    (pcase-let ((`(,old ,new) names))
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert old " " old "bar " old "\n" old)
        (goto-char (point-min))
        (let ((end (line-end-position)))
          (cl-letf (((symbol-function 'window-start) (lambda (&rest _) 1))
                    ((symbol-function 'window-end) (lambda (&rest _) end))
                    ((symbol-function 'read-string) (lambda (&rest _) new)))
            (my-symbol-overlay-rename-visible)))
        (should (equal (buffer-string) (concat new " " old "bar " new "\n" old)))))))

(ert-deftest my-test-audit-rename-function-boundaries ()
  :tags '(:audit)
  (dolist (new '("a_longer_name" "x"))
    (with-temp-buffer
      (c-mode)
      (insert "void first() { int count; count++; }\nvoid next() { int count; }\n")
      (goto-char (point-min))
      (search-forward "count")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) new)))
        (my-symbol-overlay-rename-in-function))
      (should (equal (buffer-string)
                     (format "void first() { int %s; %s++; }\nvoid next() { int count; }\n" new new))))))

(ert-deftest my-test-audit-rename-visible-partial-symbols ()
  :tags '(:audit)
  ;; 画面の両端が識別子の途中でも、見えている部分だけを一致と見なさない。
  ;; 既に narrowing されている場合も、隠れた前後の文字を境界判定に使う。
  (dolist (narrowed '(nil t))
    (dolist (new '("x" "longer_name" ""))
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "barfoo foo foobar foo")
        (when narrowed (narrow-to-region 4 15))
        (goto-char 8)
        (cl-letf (((symbol-function 'window-start) (lambda (&rest _) 4))
                  ((symbol-function 'window-end) (lambda (&rest _) 15))
                  ((symbol-function 'read-string) (lambda (&rest _) new)))
          (my-symbol-overlay-rename-visible))
        (should (= (point-min) (if narrowed 4 1)))
        (should (= (point-max) (+ (if narrowed 15 22) (- (length new) 3))))
        (widen)
        (should (equal (buffer-string) (concat "barfoo " new " foobar foo")))))))

(ert-deftest my-test-audit-rename-preserves-narrowed-scope ()
  :tags '(:audit)
  (with-temp-buffer
    (insert "foo foo foo")
    (narrow-to-region 5 8)
    (my/symbol-overlay-replace-region "foo" "longer_name" 1 12)
    (should (= (point-min) 5))
    (should (= (point-max) 16))
    (widen)
    (should (equal (buffer-string) "foo longer_name foo"))))

(ert-deftest my-test-audit-rename-atomic-error ()
  :tags '(:audit)
  (with-temp-buffer
    (insert "foo foo")
    (goto-char 1)
    (let ((original (symbol-function 'replace-match)) (calls 0))
      (cl-letf (((symbol-function 'window-start) (lambda (&rest _) 1))
                ((symbol-function 'window-end) (lambda (&rest _) (point-max)))
                ((symbol-function 'read-string) (lambda (&rest _) "bar"))
                ((symbol-function 'replace-match)
                 (lambda (&rest args)
                   (if (= (cl-incf calls) 2) (error "故障注入")
                     (apply original args)))))
        (should-error (my-symbol-overlay-rename-visible)))
      (should (equal (buffer-string) "foo foo")))))

(ert-deftest my-test-audit-rename-no-symbol ()
  :tags '(:audit)
  (with-temp-buffer
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "bar")))
      (should-error (my-symbol-overlay-rename-visible) :type 'user-error))))

(ert-deftest my-test-audit-global-source-colons ()
  :tags '(:audit)
  (dolist (file '("./main.cpp" "./dir%3a12%3a/main%25.cpp"))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _in _out _display &rest args)
                 (should (member "--encode-path=:%" args))
                 (insert file ":12:printf(\"host:8080:ready\");\n") 0)))
      (let* ((item (car (my/gtags--run "-d" "symbol")))
             (loc (xref-item-location item)))
        (should (= (xref-file-location-line loc) 12))
        (should (equal (xref-file-location-file loc)
                       (expand-file-name
                        (if (equal file "./main.cpp") file "./dir:12:/main%.cpp")
                        (my/gtags--project-root))))))))

(ert-deftest my-test-audit-global-process-failure ()
  :tags '(:audit)
  (dolist (status '(2 "killed"))
    (cl-letf (((symbol-function 'call-process) (lambda (&rest _) status)))
      (should-error (my/gtags--run "-d" "symbol") :type 'user-error))))

(ert-deftest my-test-audit-directory-completion-without-io ()
  :tags '(:audit)
  (cl-letf (((symbol-function 'file-directory-p) (lambda (&rest _) (error "I/O禁止"))))
    (should (equal (my/vertico-sort-directories-first '("a-file" "z-dir/" "b-dir/"))
                   '("b-dir/" "z-dir/" "a-file")))))

(ert-deftest my-test-audit-copy-format-and-clipboard ()
  :tags '(:audit)
  (let ((buffer-file-name "/tmp/test-%s.c") (kill-ring nil) copied
        (select-enable-clipboard t))
    (let ((interprogram-cut-function (lambda (text &optional _) (setq copied text))))
      (my/copy-file-name)
      (should (equal (car kill-ring) "test-%s.c"))
      (should (equal copied "test-%s.c")))))

(ert-deftest my-test-audit-package-candidates ()
  :tags '(:audit)
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt collection &rest _)
               (should (member "package" (all-completions "package" collection))) "package")))
    (should-not (call-interactively #'my/should-use-straight))))

(ert-deftest my-test-audit-core-library-classification ()
  :tags '(:audit)
  (dolist (library '(simple subr-x package cl-lib))
    (should-not (my/should-use-straight library)))
  (should (my/should-use-straight 'my-nonexistent-external-library)))

(ert-deftest my-test-audit-window-input ()
  :tags '(:audit)
  (save-window-excursion
    (delete-other-windows)
    (let ((current-prefix-arg nil))
      (call-interactively #'my/split-window-horizontally-n))
    (should (= (length (window-list)) 2))
    (cl-letf (((symbol-function 'read-key) (lambda (&rest _) 'left)))
      (my/window-resizer))
    (dolist (n '(0 1 -1))
      (should-error (my/split-window-horizontally-n n) :type 'user-error)
      (should-error (my/split-window-vertically-n n) :type 'user-error))))

(ert-deftest my-test-audit-nvm-version-boundary ()
  :tags '(:audit)
  (let ((root (make-temp-file "my-test-nvm-" t)) (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "alias" root))
          (dolist (v '("v22.1.0" "v22.1.9" "v22.10.0" "v220.0.0"))
            (make-directory (expand-file-name (concat "versions/node/" v "/bin") root) t))
          (setenv "NVM_DIR" root)
          (dolist (entry '(("v22.1" . "v22.1.9") ("22" . "v22.10.0") ("22.1.0" . "v22.1.0")))
            (with-temp-file (expand-file-name "alias/default" root) (insert (car entry)))
            (let (selected)
              (cl-letf (((symbol-function 'my/copilot--prepend-to-path)
                         (lambda (dir) (when (string-prefix-p root dir) (setq selected dir)))))
                (my/copilot-setup-node-path))
              (should (equal selected (expand-file-name (concat "versions/node/" (cdr entry) "/bin") root))))))
      (delete-directory root t))))

(ert-deftest my-test-audit-non-native-early-init ()
  :tags '(:audit)
  ;; 変数自体が存在しないビルドを再現し、ハーネスで補わない。
  (cl-progv '(native-comp-eln-load-path) nil
    (load (expand-file-name "early-init.el" user-emacs-directory) nil t)))

(ert-deftest my-test-audit-rename-single-undo ()
  :tags '(:audit)
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "foo foobar foo")
    (undo-boundary)
    (goto-char 1)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "longer")))
      (my-symbol-overlay-rename-in-function))
    (undo-boundary)
    (undo-only 1)
    (should (equal (buffer-string) "foo foobar foo"))))

(ert-deftest my-test-audit-markdown-spell-policy ()
  :tags '(:audit)
  (require 'markdown-mode)
  (require 'flyspell)
  ;; 設定フックによる有効・無効の方針を検査し、外部辞書の有無に依存させない。
  (let ((ispell-program-name "my-test-missing-ispell"))
    (cl-letf (((symbol-function 'flyspell-mode)
               (lambda (arg) (setq-local flyspell-mode (> arg 0)))))
      (with-temp-buffer
        (markdown-mode)
        (should (bound-and-true-p flyspell-mode))
        (my/flyspell-disable)
        (should (bound-and-true-p flyspell-mode))
        (insert (make-string 3001 ?a))
        (my/flyspell-disable-in-large-buffer)
        (should-not flyspell-mode))
      (with-temp-buffer
        (text-mode)
        (flyspell-mode 1)
        (my/flyspell-disable)
        (should-not flyspell-mode)))))

(ert-deftest my-test-audit-markdown-export-command ()
  :tags '(:audit)
  (require 'markdown-mode)
  (let ((file (make-temp-file "my-test-export-" nil ".html")))
    (unwind-protect
        (with-temp-buffer
          (markdown-mode)
          (insert "# 見出し\n\n本文\n")
          ;; 外部 pandoc を必要としない固定変換器。export が実効コマンドを使うかを確認。
          (let ((markdown-command "cat")
                (buffer-file-name (concat file ".md")))
            (unwind-protect
                (markdown-export file)
              (when (file-exists-p buffer-file-name) (delete-file buffer-file-name))))
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p "見出し" (buffer-string)))
            (should (string-match-p "本文" (buffer-string)))))
      (delete-file file))))

(ert-deftest my-test-audit-lint-rejects-new-warning ()
  :tags '(:audit)
  (load (expand-file-name "tests/my-lint-el.el" user-emacs-directory) nil t)
  (let* ((root (make-temp-file "my-test-lint-" t))
         (default-directory (file-name-as-directory root))
         (file "loads/inits/99-probe.el")
         (byte-compile-dest-file-function (lambda (_) (expand-file-name "probe.elc" root))))
    (unwind-protect
        (progn
          (make-directory "loads/inits" t)
          (dolist (entry '(("(setq my-test-new-free-variable 1)" . 1)
                           ("(defvar my-test-lint-value nil)\n(setq my-test-lint-value 1)" . 0)
                           ("(defun broken (" . 1)))
            (with-temp-file file
              (insert ";;; probe.el --- fixture -*- lexical-binding: t; -*-\n" (car entry)))
            (let ((command-line-args-left (list file)) result)
              (cl-letf (((symbol-function 'kill-emacs) (lambda (status &rest _) (setq result status))))
                (my-lint-el-run))
              (should (eq result (cdr entry))))))
      (delete-directory root t))))

(ert-deftest my-test-audit-ui-compiles-without-gui-api ()
  :tags '(:audit)
  ;; GUI 対応 Emacs 上でも、tty 専用ビルドの未定義 API を再現する。
  (require 'bytecomp)
  (let* ((root (make-temp-file "my-test-ui-compile-" t))
         (byte-compile-error-on-warn t)
         (byte-compile-dest-file-function
          (lambda (_) (expand-file-name "01-ui.elc" root))))
    (unwind-protect
        (cl-letf (((symbol-function 'set-fontset-font) nil)
                  ((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
          (my/setup-fonts)
          (should (byte-compile-file
                   (expand-file-name "loads/inits/01-ui.el" user-emacs-directory))))
      (delete-directory root t))))

(ert-deftest my-test-audit-auto-save-visited-keeps-trailing-whitespace ()
  :tags '(:audit)
  ;; idle 自動保存（auto-save-visited-mode のタイマー経路）は入力途中の末尾空白を消さない。
  ;; 明示保存は従来どおり削除する。
  (let* ((dir (make-temp-file "my-test-autosave-" t))
         (file (expand-file-name "sample.txt" dir))
         buffer)
    (when (timerp my/mode-line-flash-timer)
      (cancel-timer my/mode-line-flash-timer))
    (setq my/mode-line-flash-timer nil)
    (unwind-protect
        (with-current-buffer (setq buffer (find-file-noselect file))
          (setq-local require-final-newline nil)
          (insert "int x = ")
          (my/auto-save-visited-save-some-buffers :no-prompt (lambda () t))
          (should-not (buffer-modified-p))
          (should (equal (buffer-string) "int x = "))
          (should (= (point) (point-max)))
          ;; タイマー経由の保存ではモードラインを光らせない（復元タイマーが立たない）
          (should-not my/mode-line-flash-timer)
          (insert "\nint y = ")
          (save-buffer)
          (should (equal (buffer-string) "int x =\nint y ="))
          ;; 明示保存では従来どおり光らせる
          (should (timerp my/mode-line-flash-timer)))
      (when (timerp my/mode-line-flash-timer)
        ;; 復元タイマーを取り消し、その関数を直接呼んで mode-line の色を戻す
        (let ((timer my/mode-line-flash-timer))
          (cancel-timer timer)
          (funcall (timer--function timer))))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest my-test-audit-auto-save-visited-timer-wrapped ()
  :tags '(:audit)
  ;; auto-save-visited-mode を有効化するとタイマーの関数がラッパーへ差し替わり、引数は維持される。
  (let ((was auto-save-visited-mode))
    (unwind-protect
        (progn
          (auto-save-visited-mode 1)
          (should (timerp auto-save--timer))
          (should (eq (timer--function auto-save--timer)
                      #'my/auto-save-visited-save-some-buffers))
          (should (eq (car (timer--args auto-save--timer)) :no-prompt))
          (should (functionp (cadr (timer--args auto-save--timer)))))
      (auto-save-visited-mode (if was 1 -1)))))

(ert-deftest my-test-audit-dashboard-setup-draws-immediately ()
  :tags '(:audit)
  ;; after-init の実行中にフックを足しても走らないため、描画はその場で行い、
  ;; 表示とリサイズのフック登録だけを後続へ残す。ファイル引数付き起動では何もしない。
  (require 'dashboard)
  (when (get-buffer dashboard-buffer-name) (kill-buffer dashboard-buffer-name))
  (let ((command-line-args '("emacs")))
    (unwind-protect
        (progn
          (my/dashboard-setup)
          (should (memq #'dashboard-initialize emacs-startup-hook))
          (should (memq #'dashboard-resize-on-hook window-setup-hook))
          (should (memq #'dashboard-resize-on-hook window-size-change-functions))
          (with-current-buffer (get-buffer dashboard-buffer-name)
            (should (derived-mode-p 'dashboard-mode))
            (should (> (buffer-size) 0))))
      (remove-hook 'emacs-startup-hook #'dashboard-initialize)
      (remove-hook 'window-setup-hook #'dashboard-resize-on-hook)
      (remove-hook 'window-size-change-functions #'dashboard-resize-on-hook)
      (when (get-buffer dashboard-buffer-name) (kill-buffer dashboard-buffer-name))))
  (let ((command-line-args '("emacs" "file.txt")))
    (my/dashboard-setup)
    (should-not (get-buffer dashboard-buffer-name))
    (should-not (memq #'dashboard-initialize emacs-startup-hook))))

(provide 'my-test-audit)
;;; my-test-audit.el ends here
