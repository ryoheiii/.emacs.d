;;; my-test-cpp-config.el --- C++ コードリーディング設定の回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; C/C++ スタイル、LSP 起動条件、検索経路、起動時性能設定を検証する。

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'my-gtags)

;;;;; [Group] C++ Config - 編集・検索設定 ;;;;;
(ert-deftest my-test-cpp-config-google-style ()
  :tags '(:cpp-config)
  (let ((file (make-temp-file "my-test-cpp-config-" nil ".cpp"))
        buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (should (derived-mode-p 'c++-mode))
            (should (= c-basic-offset 4))
            (should (equal c-indentation-style "google"))
            (should-not indent-tabs-mode)
            (dolist (offset '((innamespace . 0)
                              (access-label . /)
                              (case-label . +)))
              (should (equal (alist-get (car offset) c-offsets-alist)
                             (cdr offset))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest my-test-cpp-config-project-search ()
  :tags '(:cpp-config)
  (should (eq (key-binding (kbd "C-x g"))
              'my/consult-ripgrep-or-grep))
  ;; consult を require せず起動直後の値を検証する
  ;; (:init で設定するため、consult の遅延ロード前から有効であること)
  (when (executable-find "rg")
    (should (eq xref-search-program 'ripgrep))))

(ert-deftest my-test-cpp-config-process-output-size ()
  :tags '(:cpp-config)
  (should (= read-process-output-max 1048576)))

;;;;; [Group] C++ Config - eglot 起動条件 ;;;;;
(defun my-test-cpp-config--eglot-called-p (dir file &optional no-clangd)
  "DIR の FILE で my/eglot-cc-maybe-ensure が eglot-ensure を呼ぶか返す.
NO-CLANGD が non-nil なら clangd 不在環境を模擬する。"
  (let (called)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (command)
                 (and (not no-clangd)
                      (string= command "clangd")
                      "/stub/clangd")))
              ((symbol-function 'eglot-ensure)
               (lambda ()
                 (setq called t))))
      (with-temp-buffer
        (when file
          (setq buffer-file-name (expand-file-name file dir)))
        (setq default-directory dir)
        (my/eglot-cc-maybe-ensure)))
    called))

(ert-deftest my-test-cpp-config-eglot-guard ()
  :tags '(:cpp-config)
  (let* ((root (make-temp-file "my-test-eglot-guard-" t))
         (with-cdb (expand-file-name "with-cdb/" root))
         (with-dot-clangd (expand-file-name "with-dot-clangd/" root))
         (without-cdb (expand-file-name "without-cdb/" root)))
    (unwind-protect
        (progn
          (make-directory with-cdb)
          (make-directory with-dot-clangd)
          (make-directory without-cdb)
          (with-temp-file (expand-file-name "compile_commands.json" with-cdb))
          (with-temp-file (expand-file-name ".clangd" with-dot-clangd))
          ;; 3 条件成立 → 起動
          (should (my-test-cpp-config--eglot-called-p with-cdb "sample.cpp"))
          ;; .clangd 単独 (CDB なし) も意図的に起動対象
          (should (my-test-cpp-config--eglot-called-p with-dot-clangd "sample.cpp"))
          ;; 各条件を独立に欠く → 起動しない
          (should-not (my-test-cpp-config--eglot-called-p without-cdb "sample.cpp"))
          (should-not (my-test-cpp-config--eglot-called-p with-cdb "sample.log"))
          (should-not (my-test-cpp-config--eglot-called-p with-cdb "sample.cpp" t))
          (should-not (my-test-cpp-config--eglot-called-p with-cdb nil)))
      (delete-directory root t))))

;;;;; [Group] C++ Config - フォーカス・タグ検索 ;;;;;
(ert-deftest my-test-cpp-config-focus-change ()
  :tags '(:cpp-config)
  (should-not focus-out-hook)
  (should (advice-function-member-p #'my/after-focus-change
                                    after-focus-change-function)))

(ert-deftest my-test-cpp-config-focus-change-edge-only ()
  "同一フォーカス状態の反復判定では保存・GC を繰り返さない."
  :tags '(:cpp-config)
  (let ((my/all-frames-unfocused-p nil)
        (my/focus-change-timer nil)
        (save-count 0))
    (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _f) nil))
              ((symbol-function 'save-some-buffers)
               (lambda (&rest _) (setq save-count (1+ save-count))))
              ((symbol-function 'garbage-collect) #'ignore))
      ;; 全フレーム喪失への遷移で 1 回だけ実行
      (my/handle-focus-change)
      (my/handle-focus-change)
      (should (= save-count 1))
      ;; フォーカス回復 → 再喪失で再度実行
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _f) t)))
        (my/handle-focus-change))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _f) nil)))
        (my/handle-focus-change))
      (should (= save-count 2)))))

(ert-deftest my-test-cpp-config-focus-change-debounce ()
  "反復通知は既存の idle timer をキャンセルして 1 本にまとめる."
  :tags '(:cpp-config)
  (let ((my/focus-change-timer nil)
        (cancel-count 0))
    (unwind-protect
        (cl-letf* ((real-cancel (symbol-function 'cancel-timer))
                   ((symbol-function 'cancel-timer)
                    (lambda (timer)
                      (setq cancel-count (1+ cancel-count))
                      (funcall real-cancel timer))))
          (my/after-focus-change)
          (should (timerp my/focus-change-timer))
          (should (= cancel-count 0))
          ;; 2 回目の通知は既存タイマーをキャンセルして再予約する
          (my/after-focus-change)
          (should (timerp my/focus-change-timer))
          (should (= cancel-count 1)))
      (when (timerp my/focus-change-timer)
        (cancel-timer my/focus-change-timer)))))

(ert-deftest my-test-cpp-config-gtags-non-lsp-fallback ()
  :tags '(:cpp-config)
  (with-temp-buffer
    (insert "sample")
    (goto-char (point-min))
    (let (global-args xref-called)
      (cl-letf (((symbol-function 'my/gtags--run)
                 (lambda (flag input)
                   (setq global-args (list flag input))
                   nil))
                ((symbol-function 'xref-find-definitions)
                 (lambda (_identifier)
                   (setq xref-called t))))
        (should-not (my/gtags--lsp-p))
        (my/gtags-find-definition)
        (should (equal global-args '("-d" "sample")))
        (should-not xref-called)))))

(defmacro my-test-cpp-config--with-gtags-stubs (lsp-p &rest body)
  "LSP-P を my/gtags--lsp-p の返り値として BODY を実行する.
BODY 内では global-calls / xref-def-calls / xref-ref-calls で呼び出しを観測できる。"
  (declare (indent 1))
  `(with-temp-buffer
     (insert "sample")
     (goto-char (point-min))
     (let (global-calls xref-def-calls xref-ref-calls)
       (cl-letf (((symbol-function 'my/gtags--lsp-p) (lambda () ,lsp-p))
                 ((symbol-function 'my/gtags--find-via-global)
                  (lambda (flag symbol)
                    (push (list flag symbol) global-calls)))
                 ((symbol-function 'xref-find-definitions)
                  (lambda (identifier)
                    (push identifier xref-def-calls)))
                 ((symbol-function 'xref-find-references)
                  (lambda (identifier)
                    (push identifier xref-ref-calls))))
         ,@body))))

(ert-deftest my-test-cpp-config-gtags-lsp-dispatch ()
  "LSP 管理下の at-point 検索は xref へ委譲し、global は呼ばない."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs t
    (my/gtags-find-definition)
    (should (equal xref-def-calls '("sample")))
    (should-not global-calls)
    (my/gtags-find-references)
    (should (equal xref-ref-calls '("sample")))
    (should-not global-calls)))

(ert-deftest my-test-cpp-config-gtags-lsp-fallback-on-user-error ()
  "LSP が user-error を投げたら global 経路へフォールバックする."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs t
    (cl-letf (((symbol-function 'xref-find-definitions)
               (lambda (_identifier) (user-error "見つからない"))))
      (my/gtags-find-definition)
      (should (equal global-calls '(("-d" "sample")))))))

(ert-deftest my-test-cpp-config-gtags-prefix-forces-global ()
  "C-u 付き (手動入力) は LSP 管理下でも global 固定."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs t
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "manual")))
      (my/gtags-find-definition '(4))
      (should (equal global-calls '(("-d" "manual"))))
      (should-not xref-def-calls))))

(provide 'my-test-cpp-config)
;;; my-test-cpp-config.el ends here
