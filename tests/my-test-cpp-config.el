;;; my-test-cpp-config.el --- C++ コードリーディング設定の回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; C/C++ スタイル、LSP 起動条件、検索経路、起動時性能設定を検証する。

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'my-gtags)

;;;;; [Group] C++ Config - 編集・検索設定 ;;;;;
;; cc-mode 側のスタイル固定。ts モードが有効な環境では c-basic-offset などが
;; 存在しないため、同等の検証は my-test-cpp-config-c-ts-indent-google-equivalent
;; が担当する（どちらか一方が必ず走る）。
(ert-deftest my-test-cpp-config-google-style ()
  :tags '(:cpp-config)
  (skip-unless (not (my/treesit-cc-grammar-ready-p 'cpp)))
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

(ert-deftest my-test-cpp-config-cc-file-mode ()
  "\".cc\" は c++-mode に割り当てる (c-mode 割当の回帰を検知する)."
  :tags '(:cpp-config)
  (should (eq (alist-get "\\.cc\\'" auto-mode-alist nil nil #'equal)
              'c++-mode)))

(ert-deftest my-test-cpp-config-grep-command ()
  "C-c g の grep 初期入力は文字列 + grep-command-position で構成する.
cons 形式は組み込み grep-default-command が型エラーになるため禁止."
  :tags '(:cpp-config)
  (should (equal grep-command "grep -nr -e  ."))
  (should (= grep-command-position 13)))

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

;;;;; [Group] C++ Config - tree-sitter 段階移行 ;;;;;
;; 文法が無い環境（会社環境・CI）では cc-mode へ自動フォールバックすることが
;; この機能の前提であるため、フォールバック側を最優先で固定する。
(ert-deftest my-test-cpp-config-treesit-fallback ()
  "cpp 文法が無い環境では c++-mode の remap を登録せず cc-mode のまま動作する.
C と C++ は独立に判定するため、ここでは cpp 側だけを検査する."
  :tags '(:cpp-config)
  (skip-unless (not (my/treesit-cc-grammar-ready-p 'cpp)))
  (should-not (alist-get 'c++-mode major-mode-remap-alist))
  (let ((file (make-temp-file "my-test-treesit-fallback-" nil ".cpp"))
        buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (should (eq major-mode 'c++-mode))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest my-test-cpp-config-treesit-remap-matrix ()
  "文法の有無ごとに remap の登録先が独立して決まる（環境非依存）."
  :tags '(:cpp-config)
  (pcase-dolist (`(,c-ok ,cpp-ok ,c-expect ,cpp-expect)
                 '((nil nil nil                nil)
                   (t   nil my/c-mode-dispatch nil)
                   (nil t   nil                my/c++-mode-dispatch)
                   (t   t   my/c-mode-dispatch my/c++-mode-dispatch)))
    (let ((major-mode-remap-alist '((c-mode . stale) (c++-mode . stale))))
      (cl-letf (((symbol-function 'my/treesit-cc-grammar-ready-p)
                 (lambda (lang) (pcase lang ('c c-ok) ('cpp cpp-ok) (_ nil)))))
        (my/treesit-cc-apply-remap))
      (should (eq (alist-get 'c-mode major-mode-remap-alist) c-expect))
      (should (eq (alist-get 'c++-mode major-mode-remap-alist) cpp-expect)))))

(ert-deftest my-test-cpp-config-treesit-opt-out ()
  "`my/use-treesit-for-cc' を nil にすると文法があっても ts を使わない."
  :tags '(:cpp-config)
  (cl-letf (((symbol-function 'my/treesit-cc-grammar-ready-p) (lambda (_lang) t)))
    (let ((my/use-treesit-for-cc nil))
      (should-not (my/treesit-cc-ready-p 'c))
      (should-not (my/treesit-cc-ready-p 'cpp))
      ;; remap 登録後に nil へ変えても振り分け関数が cc-mode を選ぶこと
      (should (eq (my-test-cpp-config--dispatch #'my/c-mode-dispatch "/tmp/sample.c")
                  'c-mode))
      (should (eq (my-test-cpp-config--dispatch #'my/c++-mode-dispatch "/tmp/sample.cpp")
                  'c++-mode)))))

(defun my-test-cpp-config--dispatch (dispatcher file)
  "FILE を訪問中として DISPATCHER を実行し、選ばれたモード関数名を返す."
  (let (called)
    (cl-letf (((symbol-function 'c-mode)      (lambda () (setq called 'c-mode)))
              ((symbol-function 'c++-mode)    (lambda () (setq called 'c++-mode)))
              ((symbol-function 'c-ts-mode)   (lambda () (setq called 'c-ts-mode)))
              ((symbol-function 'c++-ts-mode) (lambda () (setq called 'c++-ts-mode))))
      (with-temp-buffer
        (setq buffer-file-name file)
        (funcall dispatcher)
        (setq buffer-file-name nil)))
    called))

(ert-deftest my-test-cpp-config-treesit-dispatch ()
  "実ソースだけ ts モードへ回し、cc-mode を流用しているファイルは除外する.
文法の有無に依存しないよう可用性判定はスタブする."
  :tags '(:cpp-config)
  (cl-letf (((symbol-function 'my/treesit-cc-ready-p) (lambda (_lang) t)))
    ;; 実 C/C++ ソース → ts モード
    (should (eq (my-test-cpp-config--dispatch #'my/c-mode-dispatch "/tmp/sample.c")
                'c-ts-mode))
    (should (eq (my-test-cpp-config--dispatch #'my/c++-mode-dispatch "/tmp/sample.cpp")
                'c++-ts-mode))
    (should (eq (my-test-cpp-config--dispatch #'my/c++-mode-dispatch "/tmp/sample.h")
                'c++-ts-mode))
    ;; ログ閲覧用の流用 (.log/.cfg) と Squirrel (.nut) は cc-mode のまま
    (should (eq (my-test-cpp-config--dispatch #'my/c-mode-dispatch "/tmp/sample.log")
                'c-mode))
    (should (eq (my-test-cpp-config--dispatch #'my/c-mode-dispatch "/tmp/sample.cfg")
                'c-mode))
    (should (eq (my-test-cpp-config--dispatch #'my/c++-mode-dispatch "/tmp/sample.nut")
                'c++-mode))
    ;; ファイルに紐付かないバッファも cc-mode 側へ倒す
    (should (eq (my-test-cpp-config--dispatch #'my/c-mode-dispatch nil) 'c-mode))
    (should (eq (my-test-cpp-config--dispatch #'my/c++-mode-dispatch nil) 'c++-mode))))

(ert-deftest my-test-cpp-config-treesit-grammar-dir-isolated ()
  "文法の配置先は var/package/ 配下へ隔離し、リポジトリ直下へ置かない."
  :tags '(:cpp-config)
  (skip-unless (and (fboundp 'treesit-available-p) (treesit-available-p)))
  ;; 実ディレクトリは導入時まで存在しないため文字列前方一致で判定する
  (should (string-prefix-p (file-name-as-directory (my-set-package ""))
                           my/treesit-grammar-dir))
  (should (member my/treesit-grammar-dir treesit-extra-load-path)))

(ert-deftest my-test-cpp-config-c-ts-indent-google-equivalent ()
  "ts モードのインデントが google-c-style 相当（offset 4）になること."
  :tags '(:cpp-config)
  (skip-unless (my/treesit-cc-grammar-ready-p 'cpp))
  (require 'c-ts-mode)
  (with-temp-buffer
    (c++-ts-mode)
    (insert "namespace ns {\n"
            "class A {\n"
            "public:\n"
            "void f(int x) {\n"
            "switch (x) {\n"
            "case 1:\n"
            "break;\n"
            "}\n"
            "}\n"
            "};\n"
            "}\n")
    (indent-region (point-min) (point-max))
    (goto-char (point-min))
    (let ((column-of
           (lambda (needle)
             (goto-char (point-min))
             (should (re-search-forward (concat "^\\([ \t]*\\)" (regexp-quote needle)) nil t))
             (length (match-string 1)))))
      ;; (innamespace . 0): namespace 本体はインデントしない
      (should (= (funcall column-of "class A {") 0))
      ;; (access-label . /): メンバ (4) より半段浅い 2
      (should (= (funcall column-of "public:") 2))
      (should (= (funcall column-of "void f(int x) {") 4))
      ;; (case-label . +): switch から 1 段下げる
      (should (= (funcall column-of "switch (x) {") 8))
      (should (= (funcall column-of "case 1:") 12))
      (should (= (funcall column-of "break;") 16)))))

;;;;; [Group] C++ Config - 補完フォールバック段 ;;;;;
(ert-deftest my-test-cpp-config-irony-server-prefix ()
  "irony の導入先は var/hist/ 配下で、可用性判定と同じ場所を指すこと."
  :tags '(:cpp-config)
  (should (string-prefix-p (file-name-as-directory (my-set-history ""))
                           my/irony-server-prefix))
  ;; 遅延パッケージの :custom は theme 値として記録され、パッケージのロード時に
  ;; 適用される。実ロードして反映と :config の設定を確認する
  ;; (irony 未ロードを固定する :invariant とは別プロセスで動くため安全)。
  (require 'irony)
  (should (equal irony-server-install-prefix my/irony-server-prefix))
  ;; ts モードでも irony を有効化でき、clang へ渡す言語指定も落ちないこと
  (should (memq 'c-ts-mode irony-supported-major-modes))
  (should (memq 'c++-ts-mode irony-supported-major-modes))
  (should (equal (alist-get 'c-ts-mode irony-lang-compile-option-alist) "c"))
  (should (equal (alist-get 'c++-ts-mode irony-lang-compile-option-alist) "c++")))

(ert-deftest my-test-cpp-config-irony-server-detection ()
  "irony-server の探索は導入先の bin/ を見る（PATH 上の実体も拾える）."
  :tags '(:cpp-config)
  ;; PATH 上に実体があると「不在」の検査が成立しないため、その環境では skip する
  (skip-unless (not (executable-find "irony-server")))
  (let* ((root (make-temp-file "my-test-irony-" t))
         (bin (expand-file-name "bin" root))
         (exe (expand-file-name "irony-server" bin)))
    (unwind-protect
        (let ((my/irony-server-prefix root))
          (should-not (my/irony-server-available-p))
          (make-directory bin)
          (with-temp-file exe (insert "#!/bin/sh\n"))
          (set-file-modes exe #o755)
          (should (my/irony-server-available-p)))
      (delete-directory root t))))

(ert-deftest my-test-cpp-config-irony-gate ()
  "irony-server 実体が無い環境では irony-mode を有効化しない."
  :tags '(:cpp-config)
  (let (enabled)
    (cl-letf (((symbol-function 'irony-mode) (lambda (&rest _) (setq enabled t))))
      (cl-letf (((symbol-function 'my/irony-server-available-p) (lambda () nil)))
        (my/irony-maybe-enable)
        (should-not enabled))
      (cl-letf (((symbol-function 'my/irony-server-available-p) (lambda () t)))
        (my/irony-maybe-enable)
        (should enabled)))))

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
