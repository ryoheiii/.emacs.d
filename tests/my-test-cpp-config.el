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

(ert-deftest my-test-cpp-config-treesit-standard-install-path ()
  "標準の `treesit-install-language-grammar' が同じレシピと導入先を使う."
  :tags '(:cpp-config)
  ;; レシピはグローバルへ載せる（自作コマンドを知らなくても標準コマンドで入る）
  (dolist (lang '(c cpp))
    (let ((recipe (alist-get lang treesit-language-source-alist)))
      (should recipe)
      ;; ABI 互換のためタグを固定する（追随は手動）
      (should (string-match-p "\\`v[0-9]" (nth 1 recipe)))))
  ;; 標準コマンドの既定 OUT-DIR。空だとリポジトリ直下へ書き出してしまう。
  (should (equal (car treesit--install-language-grammar-out-dir-history)
                 my/treesit-grammar-dir)))

(ert-deftest my-test-cpp-config-treesit-sources-merge ()
  "レシピの登録は追記であり、他言語の登録を消さない."
  :tags '(:cpp-config)
  ;; treesit.el 未ロードの環境では defcustom が無く let が静的束縛になるため dlet を使う
  (dlet ((treesit-language-source-alist
          '((sentinel . ("https://example.invalid/sentinel" "v0.0.1" "src")))))
    (my/treesit-register-c-sources)
    (should (alist-get 'sentinel treesit-language-source-alist))
    (should (alist-get 'c treesit-language-source-alist))
    (should (alist-get 'cpp treesit-language-source-alist))))

(ert-deftest my-test-cpp-config-treesit-requires-outdir-support ()
  "導入先を指定できない Emacs 29 以前では文法導入を実行しない.
`treesit-install-language-grammar' の OUT-DIR 引数は Emacs 30 以降にしか無く、
29 で呼ぶと引数エラーになるうえ、通ってもリポジトリ直下へ書き出してしまう。"
  :tags '(:cpp-config)
  (cl-letf (((symbol-function 'treesit-available-p) (lambda () t)))
    ;; Emacs 29 相当 (lang のみ)
    (cl-letf (((symbol-function 'treesit-install-language-grammar) (lambda (_lang) nil)))
      (should-not (my/treesit-grammars-installable-p))
      (should-error (my/treesit-install-c-grammars) :type 'user-error))
    ;; Emacs 30 相当 (lang + out-dir)
    (cl-letf (((symbol-function 'treesit-install-language-grammar)
               (lambda (_lang &optional _out-dir) nil)))
      (should (my/treesit-grammars-installable-p)))))

(ert-deftest my-test-cpp-config-treesit-install-detects-failure ()
  "導入の失敗を握り潰さない.
`treesit-install-language-grammar' は失敗しても display-warning を出すだけで
正常終了する。再ビルド時は古い文法が残るため可用性判定も真のままになる。"
  :tags '(:cpp-config)
  (let ((warning-minimum-level :emergency))
    (cl-letf (((symbol-function 'treesit-language-available-p) (lambda (_lang &optional _d) t)))
      ;; 警告が出た実行は失敗として扱う
      (cl-letf (((symbol-function 'treesit-install-language-grammar)
                 (lambda (_lang &optional _out-dir)
                   (display-warning 'treesit "test failure"))))
        (should-not (my/treesit-install-one-grammar 'c)))
      ;; 警告が出なければ成功
      (cl-letf (((symbol-function 'treesit-install-language-grammar)
                 (lambda (_lang &optional _out-dir) nil)))
        (should (my/treesit-install-one-grammar 'c))))))

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
      (should (= (funcall column-of "break;") 16))))
  ;; C/C++ 共通の構造体でも、標準規則と差分規則が両方効くこと。
  (dolist (mode (if (my/treesit-cc-grammar-ready-p 'c)
                   '(c-ts-mode c++-ts-mode)
                 '(c++-ts-mode)))
    (with-temp-buffer
      (funcall mode)
      (insert "struct A {\nint value;\n};\nint f(int x) {\nswitch (x) {\n"
              "case 1:\nreturn x;\n}\nreturn 0;\n}\n")
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string)
                     "struct A {\n    int value;\n};\nint f(int x) {\n    switch (x) {\n        case 1:\n            return x;\n    }\n    return 0;\n}\n")))))

;;;;; [Group] C++ Config - ts モードの入力途中（ERROR 状態）のインデント ;;;;;
;; 波括弧が 2 段以上開いていると tree-sitter は木全体を ERROR へ落とし、既定の
;; 規則は桁 0 へ倒す。括弧の深さから算出する経路を固定する。

(ert-deftest my-test-cpp-config-c-ts-error-indent ()
  "構文木が ERROR の入力途中でも cc-mode + google-c-style と同じ桁になること."
  :tags '(:cpp-config)
  (skip-unless (my/treesit-cc-grammar-ready-p 'cpp))
  (require 'c-ts-mode)
  ;; (最後の行の期待値 . バッファ内容)
  (pcase-dolist (`(,expected . ,text)
                 '(;; 空行では node が nil・parent が root になる。(parent-is "ERROR")
                   ;; では捕まらないため、ここが桁 0 へ落ちていた
                   (8 . "int f() {\n    if (x) {\n")
                   (8 . "int f() {\n    if (x) {\n        y;\n")
                   ;; 行頭の閉じ波括弧は 1 段戻す
                   (4 . "int f() {\n    if (x) {\n        y;\n}")
                   ;; access_specifier は ERROR 状態でも半段（規則の順序が効く）
                   (2 . "class A {\n    public:")
                   ;; namespace 本体はインデントしない (innamespace . 0)
                   (0 . "namespace ns {\n")
                   (4 . "namespace ns {\nclass C {\n")
                   ;; extern \"C\" は google-c-style が指定せず cc-mode 既定の +
                   (4 . "extern \"C\" {\n")
                   ;; namespace 本体の `{' だけを除外する。`{' の直前の語で判定するため
                   ;; 同じ行に続く別の `{' や namespace 別名を取り違えない
                   (4 . "namespace ns { class C {\n")
                   (4 . "namespace alias = target; class C {\n")
                   ;; `{' を次行へ置いた配置でも namespace 本体と判定する
                   (0 . "namespace ns\n{\n")
                   ;; 直前がコメントでも壊れた木を見落とさないこと
                   ;; （コメントは ERROR の外側へ付くことがある）
                   (8 . "int f() {\n    if (x) {\n        // c\n")
                   (8 . "int f() {\n    if (x) {\n        /* c */\n")))
    (with-temp-buffer
      (c++-ts-mode)
      (insert text)
      (goto-char (point-max))
      (treesit-indent)
      (should (equal (cons (current-indentation) text)
                     (cons expected text))))))

(ert-deftest my-test-cpp-config-c-ts-error-indent-scope ()
  "壊れているのが木の内側だけなら既定規則へ譲ること.
桁を肩代わりするのは root 直下の ERROR が末尾を飲み込んだ場合と、PARENT が
ERROR そのものの場合に限る。関数本体の中だけが壊れていて既定の規則が正しい桁を
出せるなら、そちらを使う。"
  :tags '(:cpp-config)
  (skip-unless (my/treesit-cc-grammar-ready-p 'cpp))
  (require 'c-ts-mode)
  ;; (期待する桁 対象行（0 起点） . バッファ内容)
  (pcase-dolist (`(,expected ,line . ,text)
                 '((8 2 . "int f() {\n    if (x)\n\n        broken = ;\n}")
                   (8 2 . "int f() {\n    while (x)\n\n        broken = ;\n}")
                   ;; for だけは PARENT 自体が ERROR になり既定の規則も桁を
                   ;; 決められない。波括弧が無い本体は括弧の深さから見えないため
                   ;; 4 になる（cc-mode は 8）。移行前からの既知の差分。
                   (4 2 . "int f() {\n    for (i)\n\n        broken = ;\n}")
                   ;; 壊れていない木の空行（対照）
                   (8 2 . "int f() {\n    if (x) {\n\n        y;\n    }\n}")))
    (with-temp-buffer
      (c++-ts-mode)
      (insert text)
      (goto-char (point-min))
      (forward-line line)
      (treesit-indent)
      (should (equal (list (current-indentation) line text)
                     (list expected line text))))))

(ert-deftest my-test-cpp-config-c-ts-error-indent-preserves-point ()
  "ERROR 状態のインデント判定が point を動かさないこと.
`syntax-ppss' は POS を渡すと point を POS へ残す。インデント関数が巻き戻さないと
以降の自己挿入が行頭へ入りバッファが壊れるため、専用ラッパで閉じ込めている。"
  :tags '(:cpp-config)
  (with-temp-buffer
    ;; 構文テーブルだけを使う経路なので文法の有無に依存しない
    (c++-mode)
    (insert "int f() {if (x) {y;")
    (goto-char (point-max))
    (let ((origin (point))
          (bol (line-beginning-position)))
      (should-not (= origin bol))          ; 移動が起きれば検出できる配置
      (my/c-ts--ppss bol)
      (should (= (point) origin))
      (my/c-ts-error-offset nil nil bol)
      (should (= (point) origin))
      (my/c-ts--non-indenting-open-p (1+ (string-match "{" (buffer-string))))
      (should (= (point) origin)))))

(ert-deftest my-test-cpp-config-c-ts-error-context-preserves-point ()
  "ERROR 判定（木を見る経路）が point を動かさないこと.
`my/c-ts--toplevel-error-p' は `forward-comment' で戻るため文法が要る。"
  :tags '(:cpp-config)
  (skip-unless (my/treesit-cc-grammar-ready-p 'cpp))
  (require 'c-ts-mode)
  (with-temp-buffer
    (c++-ts-mode)
    (insert "int f() {\n    if (x) {\n        // c\n        y;")
    (goto-char (point-max))
    (let ((origin (point))
          (bol (line-beginning-position)))
      (should-not (= origin bol))
      (my/c-ts-error-context-p nil nil bol)
      (should (= (point) origin))
      (my/c-ts--toplevel-error-p bol)
      (should (= (point) origin)))))

(ert-deftest my-test-cpp-config-c-ts-error-offset ()
  "括弧の深さから桁を算出し、namespace の波括弧だけ段数へ数えないこと."
  :tags '(:cpp-config)
  ;; (期待する桁 . バッファ内容)。point-max の行を対象にする
  (pcase-dolist (`(,expected . ,text)
                 '((0 . "int x = 1;\n")
                   (4 . "int f() {\n")
                   (8 . "int f() {\n    if (x) {\n")
                   (12 . "int f() {\n    if (x) {\n        while (y) {\n")
                   ;; 行頭が閉じ括弧なら 1 段戻す
                   (4 . "int f() {\n    if (x) {\n}")
                   ;; namespace は数えない。入れ子・無名・`::' 付き・次行の `{' も同じ
                   (0 . "namespace ns {\n")
                   (0 . "namespace a {\nnamespace b {\n")
                   (0 . "namespace {\n")
                   (0 . "namespace a::b {\n")
                   (0 . "inline namespace v1 {\n")
                   (0 . "namespace ns\n{\n")
                   (4 . "namespace ns {\nclass C {\n")
                   ;; namespace 本体以外の `{' は数える（直前の語で判定する）
                   (4 . "namespace ns { class C {\n")
                   (4 . "namespace alias = target; class C {\n")
                   (4 . "struct namespace_holder {\n")
                   ;; extern \"C\" は数える
                   (4 . "extern \"C\" {\n")))
    (with-temp-buffer
      (c++-mode)
      (insert text)
      (goto-char (point-max))
      (should (equal (cons (my/c-ts-error-offset nil nil (line-beginning-position)) text)
                     (cons expected text))))))

;;;;; [Group] C++ Config - ts モードの自動改行と hungry delete ;;;;;
;; cc-mode の c-toggle-auto-hungry-state 相当を組み込み機能で再現している。
;; 判定は構文テーブルと行内容だけを見るため、文法が無い環境でも検証できる
;; （c++-mode バッファを使う）。
(ert-deftest my-test-cpp-config-c-ts-hungry-delete ()
  "連続する空白と改行をまとめて削除し、リテラル内では 1 文字に留める."
  :tags '(:cpp-config)
  (with-temp-buffer
    (c++-mode)
    (insert "int x;\n    ")
    (my/c-ts-hungry-delete-backward)
    (should (equal (buffer-string) "int x;")))
  (with-temp-buffer
    (c++-mode)
    (insert "/* comment    ")
    (my/c-ts-hungry-delete-backward)
    (should (equal (buffer-string) "/* comment   ")))
  ;; 前置引数付きは通常の 1 文字削除へ戻す
  (with-temp-buffer
    (c++-mode)
    (insert "int x;\n    ")
    (my/c-ts-hungry-delete-backward t)
    (should (equal (buffer-string) "int x;\n   "))))

(ert-deftest my-test-cpp-config-c-ts-layout-rules ()
  "自動改行の規則がリテラル・行途中・括弧内・アクセス指定子を正しく区別する."
  :tags '(:cpp-config)
  (with-temp-buffer
    (c++-mode)
    (insert "int x;")
    (should (eq (my/c-ts-layout-open-brace) 'after))
    (should (eq (my/c-ts-layout-close-brace) 'around))
    (should (eq (my/c-ts-layout-semi) 'after)))
  ;; 行途中では改行しない（cc-mode の c-semi&comma-no-newlines-before-nonblanks 相当）
  (with-temp-buffer
    (c++-mode)
    (insert "foo; bar")
    (goto-char (+ (point-min) 4))
    (should-not (my/c-ts-layout-semi))
    (should-not (my/c-ts-layout-open-brace))
    (should-not (my/c-ts-layout-close-brace)))
  ;; 括弧の中では `;' で改行しない（for の区切り）
  (with-temp-buffer
    (c++-mode)
    (insert "for (int i = 0;")
    (should (my/c-ts-inside-parens-p))
    (should-not (my/c-ts-layout-semi)))
  ;; 空の `{}' 対は後ろだけ改行する（empty-defun-braces 相当）
  (with-temp-buffer
    (c++-mode)
    (insert "int f() {}")
    (should (eq (my/c-ts-layout-close-brace) 'after)))
  ;; 前置引数付きの self-insert では改行しない（cc-mode の電気コマンドと同じ）
  (with-temp-buffer
    (c++-mode)
    (insert "int x;")
    (let ((current-prefix-arg 2))
      (should-not (my/c-ts-layout-semi))
      (should-not (my/c-ts-layout-open-brace))
      (should-not (my/c-ts-layout-close-brace))))
  (with-temp-buffer
    (c++-mode)
    (insert "  public:")
    (should (eq (my/c-ts-layout-colon) 'after))
    (let ((current-prefix-arg 1))
      (should-not (my/c-ts-layout-colon))))
  ;; 文字列・コメントの中では改行しない
  ;; （C の文字列は行をまたげないため、閉じていない文字列は cc-mode の
  ;;   syntax-propertize がリテラル扱いしない。閉じた文字列で検証する）
  (with-temp-buffer
    (c++-mode)
    (insert "const char* s = \"a;b\";\n")
    (goto-char (point-min))
    (should (search-forward ";" nil t))   ; 文字列内の `;' の直後
    (should-not (my/c-ts-layout-semi))
    (should-not (my/c-ts-layout-close-brace)))
  (with-temp-buffer
    (c++-mode)
    (insert "// a;b\nint x;\n")
    (goto-char (point-min))
    (should (search-forward ";" nil t))   ; コメント内の `;' の直後
    (should-not (my/c-ts-layout-semi))
    (should-not (my/c-ts-layout-close-brace)))
  ;; コロンはアクセス指定子のときだけ改行する
  (pcase-dolist (`(,line . ,expected)
                 '(("  public:"          . after)
                   ("  private:"         . after)
                   ("  protected:"       . after)
                   ("    case 1:"        . nil)
                   ("  int a = b ? c :"  . nil)
                   ("  std::"            . nil)))
    (with-temp-buffer
      (c++-mode)
      (insert line)
      (should (eq (my/c-ts-layout-colon) expected)))))

(ert-deftest my-test-cpp-config-c-ts-brace-cleanup ()
  "`}' の後ろの改行を `;' / `,' / else / while / catch のときだけ取り消す."
  :tags '(:cpp-config)
  (pcase-dolist (`(,input . ,expected)
                 '(("class A {\n}\n;"      . "class A {\n};")
                   ("if (a) {\n}\nelse"    . "if (a) {\n} else")
                   ("do {\n}\nwhile"       . "do {\n} while")
                   ("try {\n}\ncatch"      . "try {\n} catch")
                   ;; list-close-comma 相当: `,' は `}' へ直付けする（空白を挟まない）
                   ("int a[] = {\n    {1}\n}\n," . "int a[] = {\n    {1}\n},")
                   ;; empty-defun-braces 相当: `{' 直後の空行の `}' を 1 行へ戻す
                   ("int f() {\n    }"     . "int f() {}")
                   ;; scope-operator 相当: 割れた `::' を繋ぎ直す
                   ("class A {\n  public:\n    :"  . "class A {\n  public::")
                   ;; 直前が `}' でなければ触らない
                   ("int a;\n;"            . "int a;\n;")
                   ("int a[] = {1\n,"      . "int a[] = {1\n,")
                   ;; 直前が `:' でなければ `:' も触らない
                   ("int a;\n:"            . "int a;\n:")))
    (with-temp-buffer
      (c++-mode)
      (insert input)
      (my/c-ts-pre-layout-fixups)
      (should (equal (buffer-string) expected))))
  ;; point の後ろに空白以外が残る行では何もしない（cc-mode の cleanup と同じ）。
  ;; 既存行の途中や行頭へ挿入したときに、前の行を巻き込ませないため。
  (pcase-dolist (`(,before ,after)
                 '(("int f() {\n}\n;" "x")
                   ("if (a) {\n}\nelse" "x")
                   ("int a[] = {\n    {1}\n}\n," "x")
                   ("class A {\n  public:\n    :" "X;")))
    (with-temp-buffer
      (c++-mode)
      (insert before)
      (save-excursion (insert after))   ; point の後ろへ残す
      (my/c-ts-pre-layout-fixups)
      (should (equal (buffer-string) (concat before after))))))

(defun my-test-cpp-config--type (string)
  "STRING を 1 文字ずつ実際のキー割当経由で入力する.
`self-insert-command' を `call-interactively' で呼ぶことで
`post-self-insert-hook'（cleanup → electric-layout → electric-indent）を通す。"
  (dolist (ch (string-to-list string))
    (let ((last-command-event ch))
      (call-interactively (or (key-binding (vector ch)) #'self-insert-command)))))

(ert-deftest my-test-cpp-config-c-ts-electric-typing ()
  "実際の入力経路（post-self-insert-hook 経由）で cc-mode と同じ構造になること.
helper の直接呼び出しでは electric-layout / electric-indent との連携を検証できない。"
  :tags '(:cpp-config)
  (skip-unless (my/treesit-cc-grammar-ready-p 'cpp))
  (require 'c-ts-mode)
  (pcase-dolist (`(,input . ,expected)
                 '(("for (int i = 0; i < n; ++i) {x;}"
                    . "for (int i = 0; i < n; ++i) {\n    x;\n}\n")
                   ("int f() {}"            . "int f() {}\n")
                   ("try {a;} catch (...) {b;}"
                    . "try {\n    a;\n} catch (...) {\n    b;\n}\n")
                   ("do {a;} while (b);"    . "do {\n    a;\n} while (b);\n")
                   ("int f(int a, int b);"  . "int f(int a, int b);\n")
                   ;; list-close-comma 相当: 閉じ波括弧の後ろのカンマを直付けする。
                   ;; ブレース初期化の `{' は cc-mode と違い次行へ送らない（意図的差分）
                   ("int a[][2] = {{1,2},{3,4}};"
                    . "int a[][2] = {\n    {\n        1,2\n    },{\n        3,4\n    }\n};\n")
                   ;; scope-operator 相当: public がマクロ・名前空間名のとき
                   ;; `public::X' が書ける。アクセス指定子の改行で割らない
                   ("class A {public:};"
                    . "class A {\n  public:\n};\n")
                   ("class A {public::X;};"
                    . "class A {\n  public::X;\n};\n")
                   ;; 入力途中で木が ERROR へ落ちる 2 段以上のネスト。
                   ;; 括弧の深さから桁を算出する経路が効いていないと崩れる
                   ("int f() {int x = 1;if (x) {x++;} else {x--;}return x;}"
                    . "int f() {\n    int x = 1;\n    if (x) {\n        x++;\n\
    } else {\n        x--;\n    }\n    return x;\n}\n")
                   ;; access_specifier は ERROR 状態でも半段 (access-label . /)
                   ("class A {public:int b_;private:int c_;};"
                    . "class A {\n  public:\n    int b_;\n  private:\n    int c_;\n};\n")))
    (with-temp-buffer
      (c++-ts-mode)
      (my-test-cpp-config--type input)
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     expected))))
  ;; 行途中への `;' 挿入で行を割らないこと
  (with-temp-buffer
    (c++-ts-mode)
    (insert "foo bar")
    (goto-char (+ (point-min) 3))
    (my-test-cpp-config--type ";")
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "foo; bar")))
  ;; DEL が ts モードでも hungry delete へ解決されること
  (with-temp-buffer
    (c++-ts-mode)
    (insert "int x;\n    ")
    (call-interactively (key-binding (kbd "DEL")))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "int x;")))
  ;; 前置引数付きの実入力は cc-mode と同じく素通しになること
  (pcase-dolist (`(,ch ,n . ,expected)
                 '((?\; 1 . "int x;")
                   (?\; 2 . "int x;;")
                   (?\{ 1 . "int x{")
                   (?\} 1 . "int x}")
                   (?\: 2 . "int x::")))
    (with-temp-buffer
      (c++-ts-mode)
      (insert "int x")
      (let ((last-command-event ch) (current-prefix-arg n))
        (call-interactively (or (key-binding (vector ch)) #'self-insert-command)))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     expected)))))

;;;;; [Group] C++ Config - 補完フォールバック段 ;;;;;
(ert-deftest my-test-cpp-config-irony-install-server-reachable ()
  "irony 未導入の環境でも M-x irony-install-server へ到達できること.
irony 本体の irony-install-server には autoload cookie が無く、かつ
my/irony-maybe-enable がサーバー未導入時のロードを止めるため、
:commands の autoload が無いと新規環境からサーバーを導入できなくなる。"
  :tags '(:cpp-config)
  (should (commandp 'irony-install-server))
  ;; irony を未ロードのまま呼べること（他テストが require 済みなら autoload は解決済み）
  (unless (featurep 'irony)
    (should (autoloadp (symbol-function 'irony-install-server)))))

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

(defmacro my-test-cpp-config--with-gtags-stubs (lsp-p &rest body)
  "LSP-P を管理状態として BODY を実行する.
Global、xref、候補取得、補完入力の呼び出しをそれぞれ *-calls で観測する。"
  (declare (indent 1))
  `(let ((root (make-temp-file "my-test-gtags-" t)))
     (unwind-protect
         (with-temp-buffer
           (setq default-directory (file-name-as-directory root))
           (with-temp-file (expand-file-name "GTAGS" root))
           (insert "sample")
           (goto-char (point-min))
           (let (global-calls xref-def-calls xref-ref-calls
                 candidate-calls read-calls)
             (cl-letf (((symbol-function 'my/gtags--lsp-p) (lambda () ,lsp-p))
                       ((symbol-function 'executable-find)
                        (lambda (name) (and (equal name "global") "/stub/global")))
                       ((symbol-function 'my/gtags--candidates)
                        (lambda (flag)
                          (push flag candidate-calls)
                          '("sample" "memory_pool_allocate" "cache_release")))
                       ((symbol-function 'completing-read)
                        (lambda (prompt collection &optional predicate require-match
                                        initial history default &rest _)
                          (push (list prompt collection predicate require-match
                                      initial history default) read-calls)
                          (or initial default "")))
                       ((symbol-function 'my/gtags--find-via-global)
                        (lambda (flag symbol)
                          (push (list flag symbol) global-calls)))
                       ((symbol-function 'xref-find-definitions)
                        (lambda (identifier)
                          (push identifier xref-def-calls)))
                       ((symbol-function 'xref-find-references)
                        (lambda (identifier)
                          (push identifier xref-ref-calls))))
               ,@body)))
       (delete-directory root t))))

(ert-deftest my-test-cpp-config-gtags-non-lsp-fallback ()
  "非 LSP 環境では補完した名前を Global へ渡す."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs nil
    (my/gtags-find-definition)
    (should (equal global-calls '(("-d" "sample"))))
    (should (= (length read-calls) 1))
    (should-not xref-def-calls)))

(ert-deftest my-test-cpp-config-gtags-command-completion ()
  "全検索が種類に応じた候補と編集可能な初期入力を提供する."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs nil
    (pcase-dolist (`(,command ,flag)
                  '((my/gtags-find-definition "-d")
                    (my/gtags-find-references "-r")
                    (my/gtags-find-symbol "-s")
                    (my/gtags-find-file "-P")))
      (funcall command)
      (should (equal (car candidate-calls) flag))
      (should (equal (car global-calls) (list flag "sample")))
      (should (equal (nth 4 (car read-calls)) "sample"))
      ;; 未登録名や Global の正規表現もそのまま検索できる。
      (should-not (nth 3 (car read-calls)))
      (should (member "memory_pool_allocate"
                      (all-completions "" (nth 1 (car read-calls))))))
    (should (= (length read-calls) 4))
    ;; シンボル名とファイル名の履歴は混ぜない。
    (let ((file-history (nth 5 (nth 0 read-calls)))
          (symbol-history (nth 5 (nth 1 read-calls))))
      (should (symbolp file-history))
      (should (symbolp symbol-history))
      (should file-history)
      (should symbol-history)
      (should-not (eq file-history symbol-history))
      (should (eq symbol-history (nth 5 (nth 2 read-calls))))
      (should (eq symbol-history (nth 5 (nth 3 read-calls)))))))

(ert-deftest my-test-cpp-config-gtags-orderless-completion ()
  "候補全体へ途中一致と順不同の複数語絞り込みが効く."
  :tags '(:cpp-config)
  (require 'orderless)
  (my-test-cpp-config--with-gtags-stubs nil
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (dolist (input '("allocate" "allocate pool" "pool memory"))
                   (let ((matches (completion-all-completions
                                   input collection nil (length input))))
                     (should (equal (car matches) "memory_pool_allocate"))
                     (should-not (consp (cdr matches)))))
                 "memory_pool_allocate")))
      (my/gtags-find-definition)
      (should (equal global-calls '(("-d" "memory_pool_allocate"))))
      (should (equal candidate-calls '("-d"))))))

(ert-deftest my-test-cpp-config-gtags-no-symbol-and-free-input ()
  "カーソル位置に名前が無くても補完でき、候補外の正規表現を渡せる."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs nil
    (erase-buffer)
    (cl-letf (((symbol-function 'my/gtags--candidates) (lambda (_flag) nil))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &optional _predicate require-match
                                initial &rest _)
                 (should-not (all-completions "" collection))
                 (should-not require-match)
                 (should (member initial '(nil "")))
                 "memory_.*")))
      (my/gtags-find-references)
      (should (equal global-calls '(("-r" "memory_.*")))))))

(ert-deftest my-test-cpp-config-gtags-empty-input ()
  "空文字・空白のみの入力では検索を実行しない."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs nil
    (dolist (input '("" " " "  \t"))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) input)))
        (should-error (my/gtags-find-definition) :type 'user-error)))
    (should-not global-calls)
    (should-not xref-def-calls)))

(ert-deftest my-test-cpp-config-gtags-input-cancel ()
  "補完を C-g で中止した場合は検索せず、本文を変えない."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs t
    (set-buffer-modified-p nil)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (signal 'quit nil))))
      (should (eq (condition-case nil
                      (my/gtags-find-definition)
                    (quit 'cancelled))
                  'cancelled)))
    (should-not global-calls)
    (should-not xref-def-calls)
    (should (equal (buffer-string) "sample"))
    (should-not (buffer-modified-p))))

(ert-deftest my-test-cpp-config-gtags-lsp-dispatch ()
  "LSP 管理下で初期入力を確定した場合は xref へ委譲する."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs t
    (my/gtags-find-definition)
    (should (equal xref-def-calls '("sample")))
    (should-not global-calls)
    (my/gtags-find-references)
    (should (equal xref-ref-calls '("sample")))
    (should-not global-calls)
    (should (= (length read-calls) 2))))

(ert-deftest my-test-cpp-config-gtags-lsp-keeps-exact-default ()
  "GTAGS に前方一致名しか無くても元の名前を LSP 用候補として残す."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs t
    (cl-letf (((symbol-function 'my/gtags--candidates)
               (lambda (_flag) '("sample_more")))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &optional _predicate _require-match
                                initial &rest _)
                 (let ((candidates (all-completions "" collection)))
                   (should (equal initial "sample"))
                   (should (= (cl-count "sample" candidates :test #'equal) 1))
                   (should (member "sample_more" candidates)))
                 "sample")))
      (my/gtags-find-definition)
      (my/gtags-find-references))
    (should (equal xref-def-calls '("sample")))
    (should (equal xref-ref-calls '("sample")))
    (should-not global-calls)))

(ert-deftest my-test-cpp-config-gtags-global-keeps-indexed-candidates ()
  "Global 専用経路の候補は DB に収録された名前のままにする."
  :tags '(:cpp-config)
  (pcase-dolist (`(,lsp ,prefix) '((nil nil) (t (4))))
    (my-test-cpp-config--with-gtags-stubs lsp
      (cl-letf (((symbol-function 'my/gtags--candidates)
                 (lambda (_flag) '("sample_more")))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (should (equal (all-completions "" collection) '("sample_more")))
                   "sample_more")))
        (my/gtags-find-definition prefix))
      (should (equal global-calls '(("-d" "sample_more"))))
      (should-not xref-def-calls))))

(ert-deftest my-test-cpp-config-gtags-lsp-changed-input ()
  "LSP 管理下でも別名へ変更した入力は Global へ渡す."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs t
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "manual")))
      (my/gtags-find-definition)
      (my/gtags-find-references))
    (should (equal global-calls '(("-r" "manual") ("-d" "manual"))))
    (should-not xref-def-calls)
    (should-not xref-ref-calls)))

(ert-deftest my-test-cpp-config-gtags-lsp-fallback-on-user-error ()
  "LSP が user-error を投げたら Global 経路へフォールバックする."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs t
    (cl-letf (((symbol-function 'xref-find-definitions)
               (lambda (_identifier) (user-error "見つからない"))))
      (my/gtags-find-definition)
      (should (equal global-calls '(("-d" "sample")))))))

(ert-deftest my-test-cpp-config-gtags-prefix-forces-global ()
  "C-u 付きは入力を変えなくても LSP 管理下で Global を使う."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs t
    (my/gtags-find-definition '(4))
    (my/gtags-find-references '(4))
    (should (equal global-calls '(("-r" "sample") ("-d" "sample"))))
    (should (= (length read-calls) 2))
    (should-not xref-def-calls)
    (should-not xref-ref-calls)))

(ert-deftest my-test-cpp-config-gtags-lsp-without-global ()
  "Global コマンドや DB が無くても LSP の at-point 検索を維持する."
  :tags '(:cpp-config)
  (dolist (missing '(command database))
    (my-test-cpp-config--with-gtags-stubs t
      (let ((process-environment (copy-sequence process-environment)))
        (setenv "GTAGSDBPATH" nil)
        (setenv "GTAGSROOT" nil)
        (when (eq missing 'database)
          (delete-file (expand-file-name "GTAGS" root)))
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_name) (unless (eq missing 'command) "/stub/global")))
                  ((symbol-function 'my/gtags--candidates)
                   (lambda (_flag) (ert-fail "Global を実行した"))))
          (my/gtags-find-definition)
          (my/gtags-find-references))
        (should (equal xref-def-calls '("sample")))
        (should (equal xref-ref-calls '("sample")))
        (should (equal (all-completions "" (nth 1 (car read-calls)))
                       '("sample")))
        (should-not global-calls)))))

(ert-deftest my-test-cpp-config-gtags-lsp-availability-probe-error ()
  "Global の可用性確認が失敗しても診断後に補完入力と LSP 検索を続ける."
  :tags '(:cpp-config)
  (dolist (probe '(executable-find locate-dominating-file))
    (my-test-cpp-config--with-gtags-stubs t
      (let (diagnostics)
        (cl-letf (((symbol-function probe)
                   (lambda (&rest _) (signal 'file-error '("GTAGS probe failure"))))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) diagnostics))))
          (my/gtags-find-definition)
          (my/gtags-find-references))
        (should (= (length read-calls) 2))
        (dolist (read-call read-calls)
          (should (equal (all-completions "" (nth 1 read-call)) '("sample")))
          (should (equal (nth 4 read-call) "sample")))
        (should (equal xref-def-calls '("sample")))
        (should (equal xref-ref-calls '("sample")))
        (should (= (length diagnostics) 2))
        (dolist (diagnostic diagnostics)
          (should (string-match-p "GTAGS probe failure" diagnostic)))
        (should-not candidate-calls)
        (should-not global-calls)))))

(ert-deftest my-test-cpp-config-gtags-lsp-existing-db-error ()
  "候補 DB の失敗を診断しつつ、元の名前による LSP 検索を維持する."
  :tags '(:cpp-config)
  (dolist (error-type '(user-error file-error))
    (my-test-cpp-config--with-gtags-stubs t
      (let (diagnostics)
        (cl-letf (((symbol-function 'my/gtags--candidates)
                   (lambda (_flag) (signal error-type '("GTAGS fixture failure"))))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) diagnostics))))
          (my/gtags-find-definition))
        (should (equal xref-def-calls '("sample")))
        (should (equal (all-completions "" (nth 1 (car read-calls)))
                       '("sample")))
        (should (cl-some (lambda (text) (string-match-p "GTAGS fixture failure" text))
                         diagnostics))
        (should-not global-calls)))))

(ert-deftest my-test-cpp-config-gtags-global-existing-db-error ()
  "Global 専用経路では候補 DB の失敗を隠さず検索前に通知する."
  :tags '(:cpp-config)
  (pcase-dolist (`(,lsp ,prefix) '((nil nil) (t (4))))
    (my-test-cpp-config--with-gtags-stubs lsp
      (cl-letf (((symbol-function 'my/gtags--candidates)
                 (lambda (_flag) (user-error "GTAGS の読み取りに失敗"))))
        (should-error (my/gtags-find-definition prefix) :type 'user-error))
      (should-not read-calls)
      (should-not xref-def-calls)
      (should-not global-calls))))

(ert-deftest my-test-cpp-config-gtags-lsp-broken-db-changed-input ()
  "候補 DB の失敗後でも別名入力は Global 検索となり、そのエラーを通知する."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs t
    (cl-letf (((symbol-function 'my/gtags--candidates)
               (lambda (_flag) (user-error "GTAGS の読み取りに失敗")))
              ((symbol-function 'completing-read) (lambda (&rest _) "manual"))
              ((symbol-function 'my/gtags--find-via-global)
               (lambda (flag symbol)
                 (push (list flag symbol) global-calls)
                 (user-error "Global 検索失敗"))))
      (should-error (my/gtags-find-definition) :type 'user-error))
    (should (equal global-calls '(("-d" "manual"))))
    (should-not xref-def-calls)))

(ert-deftest my-test-cpp-config-gtags-lsp-local-result-display ()
  "LSP の結果表示だけ共通選択 UI を使い、呼び出し後は元へ戻す."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-stubs t
    (let ((before-xrefs xref-show-xrefs-function)
          (before-definitions xref-show-definitions-function))
      (cl-letf (((symbol-function 'xref-find-definitions)
                 (lambda (_identifier)
                   (should (eq xref-show-xrefs-function #'my/gtags--show))
                   (should (eq xref-show-definitions-function #'my/gtags--show)))))
        (my/gtags-find-definition))
      (should (eq xref-show-xrefs-function before-xrefs))
      (should (eq xref-show-definitions-function before-definitions)))))

(ert-deftest my-test-cpp-config-gtags-single-result-confirm ()
  "単一結果もファイル・行・本文を表示し、明示選択後にだけ移動する."
  :tags '(:cpp-config)
  (let* ((item (xref-make "int sample(void)" (xref-make-file-location
                                            "/tmp/gtags-sample.c" 7 0)))
         (alist '((display-action . window)))
         (fetch-count 0)
         selected selected-action read-called)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &optional _predicate require-match &rest _)
                 (setq read-called t)
                 (should (eq require-match t))
                 (should (= fetch-count 1))
                 (should-not selected)
                 (let ((candidates (all-completions "" collection)))
                   (should (= (length candidates) 1))
                   (should (string-match-p "gtags-sample.c" (car candidates)))
                   (should (string-match-p "7" (car candidates)))
                   (should (string-match-p "int sample" (car candidates)))
                   (car candidates))))
              ((symbol-function 'xref-pop-to-location)
               (lambda (xref &optional action)
                 (setq selected xref selected-action action)))
              ((symbol-function 'consult-xref)
               (lambda (&rest _) (ert-fail "単一結果を即ジャンプする UI へ渡した"))))
      (my/gtags--show (lambda () (cl-incf fetch-count) (list item)) alist))
    (should read-called)
    (should (eq selected item))
    (should (eq selected-action 'window))
    (should (= fetch-count 1))))

(ert-deftest my-test-cpp-config-gtags-single-result-cancel ()
  "単一結果の確認を中止しても本文へジャンプしない."
  :tags '(:cpp-config)
  (let ((item (xref-make "sample" (xref-make-file-location "/tmp/sample.c" 1 0))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'xref-pop-to-location)
               (lambda (&rest _) (ert-fail "中止後に移動した"))))
      (should (eq (condition-case nil
                      (my/gtags--show (lambda () (list item)))
                    (quit 'cancelled))
                  'cancelled)))))

(ert-deftest my-test-cpp-config-gtags-multiple-results-consult ()
  "複数の結果は既存 Consult の候補選択・プレビューへ渡す."
  :tags '(:cpp-config)
  (let* ((items (list (xref-make "first" (xref-make-file-location "/tmp/a.c" 1 0))
                      (xref-make "second" (xref-make-file-location "/tmp/b.c" 2 0))))
         (alist `((window . ,(selected-window))))
         (fetch-count 0)
         shown)
    (cl-letf (((symbol-function 'consult-xref)
               (lambda (fetcher options)
                 (setq shown (funcall fetcher))
                 (should (equal options alist)))))
      (my/gtags--show (lambda () (cl-incf fetch-count) items) alist))
    (should (equal shown items))
    (should (= fetch-count 1))))

(ert-deftest my-test-cpp-config-gtags-candidate-process-input ()
  "補完は種別ごとに空 prefix で全候補を一度だけ問い合わせる."
  :tags '(:cpp-config)
  (dolist (flag '("-d" "-r" "-s" "-P"))
    (let (calls)
      (cl-letf (((symbol-function 'call-process)
                 (lambda (program _in _out _display &rest args)
                   (should (equal program "global"))
                   (push args calls)
                   (insert "sample\nmemory_pool_allocate\n")
                   0)))
        (should (equal (sort (my/gtags--candidates flag) #'string<)
                       '("memory_pool_allocate" "sample"))))
      (should (equal calls (list (list "-c" flag "--" "")))))))

(ert-deftest my-test-cpp-config-gtags-candidate-process-status ()
  "候補の一致なしは空、実行失敗・異常終了は user-error にする."
  :tags '(:cpp-config)
  (cl-letf (((symbol-function 'call-process) (lambda (&rest _) 1)))
    (should-not (my/gtags--candidates "-d")))
  (dolist (status '(2 "killed"))
    (cl-letf (((symbol-function 'call-process) (lambda (&rest _) status)))
      (should-error (my/gtags--candidates "-d") :type 'user-error)))
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _) (signal 'file-missing '("global が無い")))))
    (should-error (my/gtags--candidates "-d") :type 'user-error)))

(ert-deftest my-test-cpp-config-gtags-real-candidates ()
  "実 GTAGS で定義・参照・未定義シンボル・ファイルの候補を区別する."
  :tags '(:gtags-integration)
  (skip-unless (and (executable-find "global") (executable-find "gtags")))
  (let ((root (make-temp-file "my-test-real-gtags-" t))
        (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory root)))
          (dolist (name '("GTAGSROOT" "GTAGSDBPATH" "GTAGSCONF" "GTAGSLABEL"))
            (setenv name nil))
          (with-temp-file (expand-file-name "sample.c" root)
            (insert "int memory_pool_allocate(void) { return external_value; }\n"
                    "int main(void) { return memory_pool_allocate(); }\n"))
          (should (zerop (call-process "gtags" nil nil nil)))
          (should (equal (sort (my/gtags--candidates "-d") #'string<)
                         '("main" "memory_pool_allocate")))
          (should (equal (my/gtags--candidates "-r") '("memory_pool_allocate")))
          (should (equal (my/gtags--candidates "-s") '("external_value")))
          (should (equal (my/gtags--candidates "-P") '("sample.c"))))
      (delete-directory root t))))

(provide 'my-test-cpp-config)
;;; my-test-cpp-config.el ends here
