;;; my-test-cpp-config.el --- C++ コードリーディング設定の回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; C/C++ スタイル、LSP 起動条件、検索経路、起動時性能設定を検証する。

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'my-gtags)

;;; 編集・検索設定
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

;;; eglot 起動条件
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

;;; tree-sitter 段階移行
;; 文法が無い環境では cc-mode へ自動フォールバックすることが
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
  "文法の配置先は var/packages/ 配下へ隔離し、リポジトリ直下へ置かない."
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

;;; ts モードの入力途中（ERROR 状態）のインデント
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

;;; ts モードの自動改行と hungry delete
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

;;; 補完フォールバック段
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
  "irony の導入先は var/packages/ 配下で、可用性判定と同じ場所を指すこと."
  :tags '(:cpp-config)
  (should (string-prefix-p (file-name-as-directory (my-set-package ""))
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

;;; フォーカス・タグ検索
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
  ;; 候補キャッシュ (DB の所在確認を含む) は別のテストで検査する。
  (let ((my/gtags-candidates-cache-size 0))
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
        (should (equal calls (list (list "-c" flag "--" ""))))))))

(ert-deftest my-test-cpp-config-gtags-candidate-process-status ()
  "候補の一致なしは空、実行失敗・異常終了は user-error にする."
  :tags '(:cpp-config)
  (let ((my/gtags-candidates-cache-size 0))
    (cl-letf (((symbol-function 'call-process) (lambda (&rest _) 1)))
      (should-not (my/gtags--candidates "-d")))
    (dolist (status '(2 "killed"))
      (cl-letf (((symbol-function 'call-process) (lambda (&rest _) status)))
        (should-error (my/gtags--candidates "-d") :type 'user-error)))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (signal 'file-missing '("global が無い")))))
      (should-error (my/gtags--candidates "-d") :type 'user-error))))

(ert-deftest my-test-cpp-config-gtags-real-candidates ()
  "実 GTAGS で定義・参照・未定義シンボル・ファイルの候補を区別する."
  ;; :cpp-config を付けて make test-cpp-config から実行する (global/gtags が無ければ skip)
  :tags '(:cpp-config :gtags-integration)
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

;;;; 候補キャッシュと ggtags 補完の共有
;; `global -c' の全件列挙は大規模 DB で 1 回 0.5 秒前後かかるため、種別と DB ごとに
;; 保持する。`call-process' をスタブし、`-p' (DB の所在) と `-c' (列挙) を引数で区別する。

(defun my-test-cpp-config--make-tag-files (dir)
  "DIR に中身の異なる GTAGS / GRTAGS / GPATH を作る."
  (dolist (name '("GTAGS" "GRTAGS" "GPATH"))
    (with-temp-file (expand-file-name name dir)
      (insert name "\n"))))

(defun my-test-cpp-config--global-count (calls option)
  "CALLS のうち OPTION (\"-p\" か \"-c\") で `global' を起動した回数を返す."
  (cl-count option calls :key #'caar :test #'equal))

(defmacro my-test-cpp-config--with-gtags-cache (&rest body)
  "候補キャッシュを隔離し、`global' を `call-process' のスタブにして BODY を実行する.
一時ディレクトリ root に GTAGS / GRTAGS / GPATH を作り、`default-directory' にする。
スタブの `-p' は db-dir (t なら呼び出し時の `default-directory'、関数なら呼び出した値、
nil なら DB なしの終了コード 3) を出力し、`-c' は c-hook を呼んでから c-output を
終了コード c-status で返す。BODY では calls (新しい順の (引数 ディレクトリ 環境))
を参照し、db-dir / c-output / c-status / c-hook を変更できる。環境は GTAGSROOT /
GTAGSDBPATH / GTAGSLIBPATH の値の組。"
  (declare (indent 0) (debug t))
  `(let ((root (file-name-as-directory (make-temp-file "my-test-gtags-cache-" t)))
         (process-environment (copy-sequence process-environment))
         (my/gtags--candidates-cache nil)
         (my/gtags--db-dirs nil)
         (my/gtags-candidates-cache-size 2)
         (db-dir t)
         (c-output "sample\nmemory_pool_allocate\n")
         (c-status 0)
         (c-hook nil)
         (calls nil))
     (unwind-protect
         (with-temp-buffer
           (setq default-directory root)
           (dolist (name '("GTAGSROOT" "GTAGSDBPATH" "GTAGSCONF" "GTAGSLABEL"
                           "GTAGSLIBPATH"))
             (setenv name nil))
           (my-test-cpp-config--make-tag-files root)
           (cl-letf (((symbol-function 'call-process)
                      (lambda (program _in _out _display &rest args)
                        (should (equal program "global"))
                        (push (list args (expand-file-name default-directory)
                                    (mapcar #'getenv '("GTAGSROOT" "GTAGSDBPATH"
                                                       "GTAGSLIBPATH")))
                              calls)
                        (pcase (car args)
                          ("-p"
                           (let ((dir (cond ((eq db-dir t) default-directory)
                                            ((functionp db-dir) (funcall db-dir))
                                            (t db-dir))))
                             (if (not dir)
                                 (progn (insert "global: GTAGS not found.\n") 3)
                               (insert (directory-file-name (expand-file-name dir)) "\n")
                               0)))
                          ("-c"
                           (when c-hook (funcall c-hook))
                           (insert c-output)
                           c-status)
                          (_ (ert-fail (format "想定外の global の引数: %S" args)))))))
             ,@body))
       (delete-directory root t))))

(defmacro my-test-cpp-config--with-remote-root (remote &rest body)
  "REMOTE が non-nil なら root 配下をリモートとみなして BODY を実行する.
TRAMP へ接続させないため `file-remote-p' だけを差し替える。"
  (declare (indent 1) (debug t))
  `(cl-letf* ((real-remote-p (symbol-function 'file-remote-p))
              ((symbol-function 'file-remote-p)
               (lambda (file &rest args)
                 (if (and ,remote (stringp file)
                          (string-prefix-p root (expand-file-name file)))
                     "/ssh:stub:"
                   (apply real-remote-p file args)))))
     ,@body))

(ert-deftest my-test-cpp-config-gtags-cache-hit ()
  "同じ種別・DB の 2 回目はタグファイルが変わらなければ global を起動しない."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-cache
    (let ((first (my/gtags--candidates "-d")))
      (should (equal first '("sample" "memory_pool_allocate")))
      (should (equal (reverse (mapcar #'car calls))
                     '(("-p") ("-c" "-d" "--" ""))))
      (should (eq (my/gtags--candidates "-d") first))
      (should (= (length calls) 2)))))

(ert-deftest my-test-cpp-config-gtags-cache-invalidate ()
  "タグファイルの更新時刻・サイズ・削除・置き換え (inode) のいずれでも取り直す."
  :tags '(:cpp-config)
  (dolist (change '(mtime size delete inode))
    (my-test-cpp-config--with-gtags-cache
      (let ((gtags (expand-file-name "GTAGS" root)))
        (should (equal (my/gtags--candidates "-d") '("sample" "memory_pool_allocate")))
        (pcase change
          ('mtime
           (set-file-times gtags (time-add (file-attribute-modification-time
                                            (file-attributes gtags))
                                           10)))
          ('size
           (write-region "more\n" nil (expand-file-name "GRTAGS" root) t 'silent))
          ('delete
           (delete-file (expand-file-name "GPATH" root)))
          ('inode
           ;; 更新時刻とサイズを保ったまま、別のファイルで置き換える。
           (let ((old (file-attributes gtags))
                 (new (expand-file-name "GTAGS.new" root)))
             (copy-file gtags new nil t)
             (rename-file new gtags t)
             (let ((attrs (file-attributes gtags)))
               (should (equal (file-attribute-modification-time attrs)
                              (file-attribute-modification-time old)))
               (should (= (file-attribute-size attrs) (file-attribute-size old)))
               (should-not (equal (file-attribute-inode-number attrs)
                                  (file-attribute-inode-number old)))))))
        (setq c-output "updated\n")
        (should (equal (my/gtags--candidates "-d") '("updated")))
        (should (= (my-test-cpp-config--global-count calls "-c") 2))
        ;; 取り直した結果は再び保持する。
        (should (equal (my/gtags--candidates "-d") '("updated")))
        (should (= (my-test-cpp-config--global-count calls "-c") 2))))))

(ert-deftest my-test-cpp-config-gtags-cache-race ()
  "列挙中にタグファイルが更新された結果は返すが保持せず、次回取り直す."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-cache
    ;; 保存時の非同期更新 (global --single-update) が列挙と重なった状況
    (setq c-hook (lambda ()
                   (setq c-hook nil)
                   (write-region "updated\n" nil (expand-file-name "GTAGS" root)
                                 t 'silent)))
    (should (equal (my/gtags--candidates "-d") '("sample" "memory_pool_allocate")))
    (should-not my/gtags--candidates-cache)
    (my/gtags--candidates "-d")
    (should (= (my-test-cpp-config--global-count calls "-c") 2))
    (should (= (length my/gtags--candidates-cache) 1))
    (my/gtags--candidates "-d")
    (should (= (my-test-cpp-config--global-count calls "-c") 2))))

(ert-deftest my-test-cpp-config-gtags-cache-requires-db ()
  "DB を特定できない、GTAGSLIBPATH が空でない、リモートのときは毎回列挙する."
  :tags '(:cpp-config)
  (dolist (condition '(no-db libpath remote))
    (my-test-cpp-config--with-gtags-cache
      (my-test-cpp-config--with-remote-root (eq condition 'remote)
        (pcase condition
          ('no-db (setq db-dir nil))
          ('libpath (setenv "GTAGSLIBPATH" "/usr/include")))
        (dotimes (_ 2)
          (should (equal (my/gtags--candidates "-d")
                         '("sample" "memory_pool_allocate"))))
        (should (= (my-test-cpp-config--global-count calls "-c") 2))
        ;; DB の所在を確かめるのは、リモートでも他 DB 依存でもない場合だけ。
        (should (= (my-test-cpp-config--global-count calls "-p")
                   (if (eq condition 'no-db) 2 0)))
        (should-not my/gtags--candidates-cache))))
  ;; 空の GTAGSLIBPATH は未設定と同じく保持する。
  (my-test-cpp-config--with-gtags-cache
    (setenv "GTAGSLIBPATH" "")
    (my/gtags--candidates "-d")
    (my/gtags--candidates "-d")
    (should (= (my-test-cpp-config--global-count calls "-c") 1))))

(ert-deftest my-test-cpp-config-gtags-cache-env-key ()
  "GTAGSROOT 単独と GTAGSDBPATH 併用は別のエントリにし、-p が返す DB で更新を判定する."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-cache
    (let* ((dbpath (file-name-as-directory (expand-file-name "db" root)))
           (root-env (directory-file-name root))
           (db-env (directory-file-name dbpath))
           (root-only (list root-env nil nil))
           (separate (list root-env db-env nil)))
      (make-directory dbpath)
      (my-test-cpp-config--make-tag-files dbpath)
      (setq db-dir (lambda () (if (getenv "GTAGSDBPATH") dbpath root)))
      (setenv "GTAGSROOT" root-env)
      (setq c-output "root_only\n")
      (should (equal (my/gtags--candidates "-d") '("root_only")))
      (setenv "GTAGSDBPATH" db-env)
      (setq c-output "separate_db\n")
      (should (equal (my/gtags--candidates "-d") '("separate_db")))
      (should (equal (mapcar #'car my/gtags--candidates-cache)
                     `(("-d" ,dbpath ,separate) ("-d" ,root ,root-only))))
      ;; -p と -c は同じディレクトリ・同じ環境で起動する。
      (should (equal (reverse calls)
                     `((("-p") ,root ,root-only)
                       (("-c" "-d" "--" "") ,root ,root-only)
                       (("-p") ,root ,separate)
                       (("-c" "-d" "--" "") ,root ,separate))))
      ;; 更新の判定は -p が返した DB のタグファイルで行う。
      (write-region "more\n" nil (expand-file-name "GTAGS" dbpath) t 'silent)
      (setq c-output "separate_db_updated\n")
      (should (equal (my/gtags--candidates "-d") '("separate_db_updated")))
      (setenv "GTAGSDBPATH" nil)
      (should (equal (my/gtags--candidates "-d") '("root_only")))
      (should (= (my-test-cpp-config--global-count calls "-c") 3)))))

(ert-deftest my-test-cpp-config-gtags-cache-keys ()
  "種別と DB ごとに保持し、上限を超えたら最も古く使ったものから捨てる."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-cache
    (let ((other (file-name-as-directory
                  (make-temp-file "my-test-gtags-cache-other-" t))))
      (unwind-protect
          (cl-flet ((c-count () (my-test-cpp-config--global-count calls "-c"))
                    (keys () (mapcar (lambda (entry) (take 2 (car entry)))
                                     my/gtags--candidates-cache)))
            (my/gtags--candidates "-d")
            (my/gtags--candidates "-r")
            (should (equal (keys) `(("-r" ,root) ("-d" ,root))))
            ;; 使ったエントリは先頭へ移る。
            (my/gtags--candidates "-d")
            (should (= (c-count) 2))
            (should (equal (keys) `(("-d" ,root) ("-r" ,root))))
            ;; 上限 2 を超えたら最も古く使った -r を捨てる。
            (my/gtags--candidates "-s")
            (should (equal (keys) `(("-s" ,root) ("-d" ,root))))
            (my/gtags--candidates "-d")
            (should (= (c-count) 3))
            (my/gtags--candidates "-r")
            (should (= (c-count) 4))
            ;; 同じ種別でも DB が違えば別エントリ。
            (my-test-cpp-config--make-tag-files other)
            (let ((default-directory other))
              (my/gtags--candidates "-r"))
            (should (= (c-count) 5))
            (should (equal (keys) `(("-r" ,other) ("-r" ,root))))
            ;; 0 で無効化し、保持中の一覧も解放する。DB の所在も確かめない。
            (let ((p-count (my-test-cpp-config--global-count calls "-p")))
              (setq my/gtags-candidates-cache-size 0)
              (my/gtags--candidates "-r")
              (should-not my/gtags--candidates-cache)
              (my/gtags--candidates "-r")
              (should-not my/gtags--candidates-cache)
              (should (= (c-count) 7))
              (should (= (my-test-cpp-config--global-count calls "-p") p-count))))
        (delete-directory other t)))))

(ert-deftest my-test-cpp-config-gtags-cache-same-db ()
  "GTAGSDBPATH で分けた DB を別のサブディレクトリから引いても 1 回の列挙を共有する."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-cache
    (let* ((src (file-name-as-directory (make-temp-file "my-test-gtags-cache-src-" t)))
           (sub-a (file-name-as-directory (expand-file-name "a" src)))
           (sub-b (file-name-as-directory (expand-file-name "b" src))))
      (unwind-protect
          (progn
            (make-directory sub-a)
            (make-directory sub-b)
            ;; ソースの祖先に GTAGS は無く、DB は root にある。
            (setenv "GTAGSROOT" (directory-file-name src))
            (setenv "GTAGSDBPATH" (directory-file-name root))
            (setq db-dir root)
            (let ((first (let ((default-directory sub-a))
                           (my/gtags--candidates "-d"))))
              (should (equal first '("sample" "memory_pool_allocate")))
              (should (eq (let ((default-directory sub-b))
                            (my/gtags--candidates "-d"))
                          first)))
            (should (= (my-test-cpp-config--global-count calls "-c") 1))
            (should (equal (mapcar #'car my/gtags--candidates-cache)
                           `(("-d" ,root (,(directory-file-name src)
                                          ,(directory-file-name root) nil)))))
            ;; DB の所在は各ディレクトリから確かめ、列挙はそのディレクトリで行う。
            (should (equal (mapcar (lambda (call) (take 2 call)) (reverse calls))
                           `((("-p") ,sub-a)
                             (("-c" "-d" "--" "") ,sub-a)
                             (("-p") ,sub-b)))))
        (delete-directory src t)))))

(ert-deftest my-test-cpp-config-gtags-cache-errors ()
  "列挙の失敗は保持せず次回やり直し、一致なし (終了コード 1) の空結果は保持する."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-gtags-cache
    (setq c-status 2)
    (should-error (my/gtags--candidates "-d") :type 'user-error)
    (should-not my/gtags--candidates-cache)
    (setq c-status 0)
    (should (equal (my/gtags--candidates "-d") '("sample" "memory_pool_allocate")))
    (should (= (my-test-cpp-config--global-count calls "-c") 2)))
  (my-test-cpp-config--with-gtags-cache
    (setq c-status 1 c-output "")
    (should-not (my/gtags--candidates "-d"))
    (should-not (my/gtags--candidates "-d"))
    (should (= (my-test-cpp-config--global-count calls "-c") 1))
    (should (equal (mapcar #'car my/gtags--candidates-cache)
                   `(("-d" ,root (nil nil nil)))))))

(ert-deftest my-test-cpp-config-gtags-cache-real-refresh ()
  "実 GTAGS を global -u で更新すると候補を取り直す (DB を環境変数で示す構成も)."
  ;; :cpp-config を付けて make test-cpp-config から実行する (global/gtags が無ければ skip)
  :tags '(:cpp-config :gtags-integration)
  (skip-unless (and (executable-find "global") (executable-find "gtags")))
  (dolist (layout '(plain gtagsroot dbpath))
    (let ((top (file-name-as-directory (make-temp-file "my-test-gtags-refresh-" t)))
          (process-environment (copy-sequence process-environment))
          (my/gtags--candidates-cache nil)
          (my/gtags--db-dirs nil)
          (my/gtags-candidates-cache-size 2))
      (unwind-protect
          (let* ((src (file-name-as-directory (expand-file-name "src" top)))
                 (db (if (eq layout 'dbpath)
                         (file-name-as-directory (expand-file-name "db" top))
                       src))
                 (source (expand-file-name "sample.c" src))
                 (gtags (expand-file-name "GTAGS" db))
                 (default-directory src))
            (dolist (name '("GTAGSROOT" "GTAGSDBPATH" "GTAGSCONF" "GTAGSLABEL"
                            "GTAGSLIBPATH"))
              (setenv name nil))
            (make-directory src)
            (make-directory db t)
            (with-temp-file source
              (insert "int first_symbol(void) { return 0; }\n"))
            (should (zerop (call-process "gtags" nil nil nil
                                         (directory-file-name db))))
            (unless (eq layout 'plain)
              (setenv "GTAGSROOT" (directory-file-name src)))
            (when (eq layout 'dbpath)
              (setenv "GTAGSDBPATH" (directory-file-name db)))
            (should (equal (my/gtags--candidates "-d") '("first_symbol")))
            (should (equal (file-truename (my/gtags--db-dir)) (file-truename db)))
            ;; gtags は秒単位で比較するため、ソースを確実に新しくしてから差分更新する。
            (with-temp-file source
              (insert "int first_symbol(void) { return 0; }\n"
                      "int second_symbol(void) { return 1; }\n"))
            (set-file-times source (time-add (file-attribute-modification-time
                                              (file-attributes gtags))
                                             2))
            (should (zerop (call-process "global" nil nil nil "-u")))
            (set-file-times gtags (time-add (file-attribute-modification-time
                                             (file-attributes gtags))
                                            2))
            (should (equal (sort (copy-sequence (my/gtags--candidates "-d")) #'string<)
                           '("first_symbol" "second_symbol"))))
        (delete-directory top t)))))

(ert-deftest my-test-cpp-config-gtags-capf-shares-cache ()
  "ggtags の補完範囲を、C-t d と共有する定義名のキャッシュで補完する."
  :tags '(:cpp-config)
  (require 'ggtags)
  (my-test-cpp-config--with-gtags-cache
    (insert "int me")
    (let ((capf (my/gtags-completion-at-point)))
      (should (= (length capf) 3))
      (should (equal (take 2 capf) '(5 7)))
      (should (equal (all-completions "" (nth 2 capf))
                     '("sample" "memory_pool_allocate")))
      (should (equal (all-completions "me" (nth 2 capf)) '("memory_pool_allocate"))))
    (should (= (my-test-cpp-config--global-count calls "-c") 1))
    (should (equal (car (car calls)) '("-c" "-d" "--" "")))
    ;; C-t d の候補取得は同じ一覧を使い、global を起動しない。
    (should (eq (my/gtags--candidates "-d") (cddr (car my/gtags--candidates-cache))))
    (should (= (my-test-cpp-config--global-count calls "-c") 1))))

(ert-deftest my-test-cpp-config-gtags-capf-delegates ()
  "C-t と別の DB を参照しうる環境と DB が無い場合は ggtags 本来の補完へ任せる."
  :tags '(:cpp-config)
  (require 'ggtags)
  (dolist (condition '(no-db remote libpath process-environment cache-disabled
                             unresolved))
    (my-test-cpp-config--with-gtags-cache
      (my-test-cpp-config--with-remote-root (eq condition 'remote)
        (let ((delegated 0))
          (cl-letf (((symbol-function 'ggtags-completion-at-point)
                     (lambda () (cl-incf delegated) 'ggtags)))
            (insert "int me")
            (pcase condition
              ('no-db
               (dolist (name '("GTAGS" "GRTAGS" "GPATH"))
                 (delete-file (expand-file-name name root))))
              ('libpath (setenv "GTAGSLIBPATH" "/usr/include"))
              ('process-environment
               (setq-local ggtags-process-environment
                           (list (concat "GTAGSDBPATH="
                                         (expand-file-name "other-db" root)))))
              ('cache-disabled (setq my/gtags-candidates-cache-size 0))
              ;; GTAGS はあるが global が DB を特定できない
              ('unresolved (setq db-dir nil)))
            (should (eq (my/gtags-completion-at-point) 'ggtags))
            (should (= delegated 1))
            (should (equal (mapcar #'car calls)
                           (and (eq condition 'unresolved) '(("-p")))))
            (should-not my/gtags--candidates-cache)))))))

(ert-deftest my-test-cpp-config-gtags-capf-global-error ()
  "打鍵中の補完では global の失敗をエラーにせず候補なしとし、次の補完で再試行する."
  :tags '(:cpp-config)
  (require 'ggtags)
  (my-test-cpp-config--with-gtags-cache
    (insert "int me")
    (setq c-status 2)
    (let ((table (nth 2 (my/gtags-completion-at-point))))
      (should table)
      (should-not (all-completions "" table))
      (should-not (all-completions "me" table))
      (should (= (my-test-cpp-config--global-count calls "-c") 1))
      (should-not my/gtags--candidates-cache))
    ;; 次の補完では改めて列挙する。
    (should-not (all-completions "" (nth 2 (my/gtags-completion-at-point))))
    (should (= (my-test-cpp-config--global-count calls "-c") 2))
    (should-not my/gtags--candidates-cache)
    (setq c-status 0)
    (should (equal (all-completions "me" (nth 2 (my/gtags-completion-at-point)))
                   '("memory_pool_allocate")))
    (should (= (my-test-cpp-config--global-count calls "-c") 3))
    (should (= (length my/gtags--candidates-cache) 1))))

(ert-deftest my-test-cpp-config-gtags-capf-replaces-ggtags ()
  "ggtags-mode の補完関数を末尾の位置のまま差し替え、無効化で取り除く."
  :tags '(:cpp-config)
  (require 'ggtags)
  (should (memq 'my/ggtags-use-cached-capf ggtags-mode-hook))
  (with-temp-buffer
    (cl-flet ((check ()
                (should (eq (car (last completion-at-point-functions))
                            #'my/gtags-completion-at-point))
                (should (= (cl-count #'my/gtags-completion-at-point
                                     completion-at-point-functions)
                           1))
                (should-not (memq #'ggtags-completion-at-point
                                  completion-at-point-functions))))
      (ggtags-mode 1)
      (check)
      ;; 再有効化でも重複しない。
      (ggtags-mode 1)
      (check)
      (ggtags-mode -1)
      (should-not (memq #'my/gtags-completion-at-point completion-at-point-functions))
      (should-not (memq #'ggtags-completion-at-point completion-at-point-functions))))
  ;; ggtags 側の補完関数が無い版では何もしない。
  (with-temp-buffer
    (add-hook 'completion-at-point-functions #'ggtags-completion-at-point t t)
    (let ((before (copy-sequence completion-at-point-functions))
          (ggtags-mode t))
      (cl-letf (((symbol-function 'ggtags-completion-at-point) nil))
        (my/gtags-use-cached-capf))
      (should (equal completion-at-point-functions before)))))

;;; LSP 階層ナビゲーション
;; my-lsp-nav.el は eglot を require しない。ここでは eglot の実物
;; (eglot--xref-make-match / eglot-uri-to-path / eglot--temp-location-buffers) を使い、
;; サーバーとの通信だけをスタブする。テストは別プロセスなので、起動経路で eglot を
;; 遅延ロードのままにする不変条件には触れない。
(require 'my-lsp-nav)
(require 'eglot)

(defmacro my-test-cpp-config--with-lsp-stubs (lsp-p responses &rest body)
  "LSP-P を管理状態、RESPONSES を (method . 応答) の alist として BODY を実行する.
応答が関数なら呼び出した結果を返す (エラーを signal させる用途)。RESPONSES は
BODY と同じ環境で評価されるため root / file / uri を参照できる。
BODY では requests (要求の (method params timeout) を新しい順に並べたリスト)、
cancels (各要求の :cancel-on-quit を requests と同じ順に並べたリスト)、
shown (表示された xref)、ref-calls (`my/gtags-find-references' への委譲引数) も
参照できる。スタブするのは jsonrpc の要求層 (`jsonrpc-request') で、
my-lsp-nav.el が eglot の内部要求ラッパーを経由しないことも固定する。"
  (declare (indent 2) (debug t))
  `(let* ((root (make-temp-file "my-test-lsp-nav-" t))
          (file (expand-file-name "a.cpp" root))
          (uri (concat "file://" file))
          requests cancels shown ref-calls)
     (ignore root file uri)
     (unwind-protect
         (progn
           (with-temp-file file
             (insert "int callee() { return 1; }\nint caller() { return callee(); }\n"))
           (cl-letf (((symbol-function 'my/gtags--lsp-p) (lambda () ,lsp-p))
                     ((symbol-function 'eglot-server-capable) (lambda (&rest _) t))
                     ((symbol-function 'eglot--current-server-or-lose) (lambda () 'server))
                     ((symbol-function 'eglot--TextDocumentPositionParams)
                      (lambda () (list :textDocument (list :uri uri)
                                       :position '(:line 1 :character 4))))
                     ((symbol-function 'eglot--TextDocumentIdentifier)
                      (lambda () (list :uri uri)))
                     ((symbol-function 'eglot--signal-textDocument/didChange) #'ignore)
                     ((symbol-function 'jsonrpc-request)
                      (lambda (_connection method params &rest rest)
                        (push (list method params (plist-get rest :timeout)) requests)
                        (push (plist-get rest :cancel-on-quit) cancels)
                        (let ((response (cdr (assq method ,responses))))
                          (if (functionp response) (funcall response) response))))
                     ((symbol-function 'xref-show-xrefs)
                      (lambda (fetcher _action) (setq shown (funcall fetcher))))
                     ((symbol-function 'my/gtags-find-references)
                      (lambda (&optional prefix) (push prefix ref-calls))))
             ,@body))
       ;; eglot--xref-make-match はダミーのサーバーを xref 先の表へ登録する
       (clrhash eglot--servers-by-xrefed-file)
       (delete-directory root t))))

(defun my-test-cpp-config--lsp-item (name line end)
  "LINE 行目の 0〜END 桁を占め、4〜10 桁が名前 NAME の階層アイテム (plist) を返す."
  (list :name name :kind 12
        :range (list :start (list :line line :character 0)
                     :end (list :line line :character end))
        :selectionRange (list :start (list :line line :character 4)
                              :end (list :line line :character 10))))

(defun my-test-cpp-config--lsp-error (code message)
  "`jsonrpc-request' が失敗したときと同じ形で `jsonrpc-error' を signal する関数を返す.
CODE が nil ならタイムアウトと同じく code 無しにする。"
  (lambda ()
    (signal 'jsonrpc-error
            (append (list "request id=1 failed:")
                    (and code (list (cons 'jsonrpc-error-code code)))
                    (list (cons 'jsonrpc-error-message message))))))

(ert-deftest my-test-cpp-config-lsp-callers ()
  "呼び出し元は incomingCalls の fromRanges を呼び出し箇所ごとに 1 件ずつ表示する."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-lsp-stubs t
      `((:textDocument/prepareCallHierarchy
         . [,(append (my-test-cpp-config--lsp-item "callee" 0 26) (list :uri uri))])
        (:callHierarchy/incomingCalls
         . [(:from ,(append (my-test-cpp-config--lsp-item "caller" 1 33) (list :uri uri))
            :fromRanges [(:start (:line 1 :character 22) :end (:line 1 :character 28))])]))
    (my/lsp-find-callers)
    (should (equal (mapcar #'car requests)
                   '(:callHierarchy/incomingCalls :textDocument/prepareCallHierarchy)))
    (should (= (length shown) 1))
    (let* ((item (car shown))
           (summary (substring-no-properties (xref-item-summary item)))
           (location (xref-item-location item)))
      (should (string-prefix-p "caller: " summary))
      (should (string-match-p "return callee();" summary))
      (should (= (xref-location-line location) 2))
      (should (file-equal-p (xref-location-group location) file)))
    ;; 一時バッファは表示後に片付ける
    (should (zerop (hash-table-count eglot--temp-location-buffers)))
    (should-not ref-calls)))

(ert-deftest my-test-cpp-config-lsp-callers-fallback ()
  "非 LSP 環境と C-u 付きは参照検索 (GNU Global 経路) へ委譲し、LSP へ要求しない."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-lsp-stubs nil nil
    (my/lsp-find-callers)
    (should (equal ref-calls '(nil)))
    (should-not requests))
  (my-test-cpp-config--with-lsp-stubs t nil
    (my/lsp-find-callers '(4))
    (should (equal ref-calls '((4))))
    (should-not requests)))

(ert-deftest my-test-cpp-config-lsp-callees ()
  "呼び出し先は outgoingCalls の to を定義名の位置で表示する."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-lsp-stubs t
      `((:textDocument/prepareCallHierarchy
         . [,(append (my-test-cpp-config--lsp-item "caller" 1 33) (list :uri uri))])
        (:callHierarchy/outgoingCalls
         . [(:to ,(append (my-test-cpp-config--lsp-item "callee" 0 26) (list :uri uri))
            :fromRanges [(:start (:line 1 :character 22) :end (:line 1 :character 28))])]))
    (my/lsp-find-callees)
    (should (= (length shown) 1))
    (let ((item (car shown)))
      (should (string-prefix-p "callee: " (substring-no-properties (xref-item-summary item))))
      (should (= (xref-location-line (xref-item-location item)) 1)))
    (should (zerop (hash-table-count eglot--temp-location-buffers)))))

(ert-deftest my-test-cpp-config-lsp-callees-requires-lsp ()
  "呼び出し先は非 LSP 環境で明示的に失敗し、Global へは倒さない."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-lsp-stubs nil nil
    (should-error (my/lsp-find-callees) :type 'user-error)
    (should-not requests)
    (should-not ref-calls)))

(ert-deftest my-test-cpp-config-lsp-method-not-found ()
  "サーバー未対応 (-32601。clangd 18 の outgoingCalls) は版の要件を含むエラーにする."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-lsp-stubs t
      `((:textDocument/prepareCallHierarchy
         . [,(append (my-test-cpp-config--lsp-item "caller" 1 33) (list :uri uri))])
        (:callHierarchy/outgoingCalls
         . ,(my-test-cpp-config--lsp-error -32601 "method not found")))
    (let ((err (should-error (my/lsp-find-callees) :type 'user-error)))
      (should (string-match-p "clangd 20" (cadr err))))))

(ert-deftest my-test-cpp-config-lsp-timeout ()
  "タイムアウト (code 無し) は原因が分かるエラーにする."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-lsp-stubs t
      `((:textDocument/prepareCallHierarchy
         . ,(my-test-cpp-config--lsp-error nil "Timed out")))
    (let ((err (should-error (my/lsp-find-callers) :type 'user-error)))
      (should (string-match-p "タイムアウト" (cadr err))))))

(ert-deftest my-test-cpp-config-lsp-request-timeout-explicit ()
  "すべての要求に有限のタイムアウトを明示する.
eglot の内部ラッパーは :timeout nil を jsonrpc-request へ転送し、既定の 10 秒を
無効化して応答しないサーバーを待ち続ける (Emacs 30.2 / 31.1 で確認)。
併せて C-g 用の :cancel-on-quit が $/cancelRequest を送ることも固定する。"
  :tags '(:cpp-config)
  (my-test-cpp-config--with-lsp-stubs t
      `((:textDocument/prepareCallHierarchy
         . [,(append (my-test-cpp-config--lsp-item "callee" 0 26) (list :uri uri))])
        (:callHierarchy/incomingCalls . nil)
        (:textDocument/switchSourceHeader . nil))
    (my/lsp-find-callers)
    (cl-letf (((symbol-function 'ff-find-other-file) #'ignore)
              ((symbol-function 'xref-push-marker-stack) #'ignore))
      (my/lsp-find-other-file))
    (my/lsp-find-workspace-symbol "callee")
    (should (equal (mapcar #'car requests)
                   '(:workspace/symbol :textDocument/switchSourceHeader
                     :callHierarchy/incomingCalls :textDocument/prepareCallHierarchy)))
    (dolist (request requests)
      (should (numberp (nth 2 request)))
      (should (> (nth 2 request) 0))
      (should (eql (nth 2 request) jsonrpc-default-request-timeout)))
    ;; C-g の中断時は eglot--request (Emacs 31) と同じく $/cancelRequest を送る
    (should (= (length cancels) (length requests)))
    (let (notified)
      (cl-letf (((symbol-function 'jsonrpc-notify)
                 (lambda (connection method params)
                   (push (list connection method params) notified))))
        (dolist (cancel cancels)
          (should (functionp cancel))
          (funcall cancel 7)))
      (should (equal notified (make-list (length cancels)
                                         '(server $/cancelRequest (:id 7))))))
    ;; eglot-advertise-cancellation が nil なら取消通知を渡さない
    (setq cancels nil)
    (let ((eglot-advertise-cancellation nil))
      (my/lsp-find-workspace-symbol "callee"))
    (should (equal cancels '(nil)))))

(ert-deftest my-test-cpp-config-lsp-timeout-real-jsonrpc ()
  "応答しないサーバーに対して jsonrpc の実タイムアウトが user-error になる.
jsonrpc-process-connection を実際に作り、要求層をスタブせずに通す。"
  :tags '(:cpp-config)
  (require 'jsonrpc)
  (let* ((process (make-process :name "my-test-lsp-stub" :command '("sleep" "30")
                                :connection-type 'pipe :noquery t))
         (connection (make-instance 'jsonrpc-process-connection
                                    :name "my-test-lsp-stub" :process process)))
    (unwind-protect
        (cl-letf (((symbol-function 'my/gtags--lsp-p) (lambda () t))
                  ((symbol-function 'eglot-server-capable) (lambda (&rest _) t))
                  ((symbol-function 'eglot--current-server-or-lose) (lambda () connection))
                  ((symbol-function 'eglot--signal-textDocument/didChange) #'ignore)
                  ((symbol-function 'eglot--TextDocumentPositionParams)
                   (lambda () '(:textDocument (:uri "file:///tmp/none.cpp")
                                :position (:line 0 :character 0)))))
          (let ((jsonrpc-default-request-timeout 0.3)
                (start (float-time)))
            (dolist (command (list #'my/lsp-find-callers
                                   (lambda () (my/lsp-find-workspace-symbol "callee"))))
              (let ((err (should-error (funcall command) :type 'user-error)))
                (should (string-match-p "タイムアウト" (cadr err)))))
            ;; 既定の 10 秒ではなく明示した短いタイムアウトで戻ること (2 要求分)
            (should (< (- (float-time) start) 5))))
      (ignore-errors (jsonrpc-shutdown connection))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest my-test-cpp-config-lsp-prepare-empty ()
  "prepare が空 (nil / 空ベクタ) なら対象が無い旨のエラーにし、次の要求を送らない."
  :tags '(:cpp-config)
  (dolist (empty (list nil []))
    (my-test-cpp-config--with-lsp-stubs t
        `((:textDocument/prepareCallHierarchy . ,empty))
      (let ((err (should-error (my/lsp-find-callers) :type 'user-error)))
        (should (string-match-p "対象がありません" (cadr err))))
      (should (= (length requests) 1)))))

(ert-deftest my-test-cpp-config-lsp-capability-missing ()
  "サーバーに capability が無ければ要求せずにエラーにする."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-lsp-stubs t nil
    (cl-letf (((symbol-function 'eglot-server-capable) (lambda (&rest _) nil)))
      (should-error (my/lsp-find-type-hierarchy) :type 'user-error)
      (should-error (my/lsp-find-callers) :type 'user-error)
      (should-not requests))))

(ert-deftest my-test-cpp-config-lsp-type-hierarchy ()
  "型階層は基底と派生を両方取得し、[基底] [派生] の順でラベルを付けて並べる."
  :tags '(:cpp-config)
  (my-test-cpp-config--with-lsp-stubs t
      `((:textDocument/prepareTypeHierarchy
         . [,(append (my-test-cpp-config--lsp-item "Derived" 1 33) (list :uri uri))])
        (:typeHierarchy/supertypes
         . [,(append (my-test-cpp-config--lsp-item "Base" 0 26) (list :uri uri))])
        (:typeHierarchy/subtypes
         . [,(append (my-test-cpp-config--lsp-item "MoreDerived" 1 33) (list :uri uri))]))
    (my/lsp-find-type-hierarchy)
    (should (equal (mapcar #'car requests)
                   '(:typeHierarchy/subtypes :typeHierarchy/supertypes
                     :textDocument/prepareTypeHierarchy)))
    (should (equal (mapcar (lambda (item)
                             (cons (substring-no-properties (xref-item-summary item) 0 4)
                                   (xref-location-line (xref-item-location item))))
                           shown)
                   '(("[基底]" . 1) ("[派生]" . 2)))))
  ;; 片方が空でも他方は表示する
  (my-test-cpp-config--with-lsp-stubs t
      `((:textDocument/prepareTypeHierarchy
         . [,(append (my-test-cpp-config--lsp-item "Base" 0 26) (list :uri uri))])
        (:typeHierarchy/supertypes . nil)
        (:typeHierarchy/subtypes
         . [,(append (my-test-cpp-config--lsp-item "Derived" 1 33) (list :uri uri))]))
    (my/lsp-find-type-hierarchy)
    (should (= (length shown) 1))
    (should (string-prefix-p "[派生]" (substring-no-properties (xref-item-summary (car shown)))))))

(ert-deftest my-test-cpp-config-lsp-other-file ()
  "ヘッダ/ソース切替は LSP の応答があればそのファイルへ、無ければ ff へ倒す."
  :tags '(:cpp-config)
  (let (visited ff-calls)
    (cl-letf (((symbol-function 'find-file)
               (lambda (filename &rest _) (push filename visited)))
              ((symbol-function 'ff-find-other-file)
               (lambda (&rest _) (push (symbol-value 'ff-always-try-to-create) ff-calls)))
              ((symbol-function 'xref-push-marker-stack) #'ignore))
      ;; LSP が対を返す
      (my-test-cpp-config--with-lsp-stubs t
          `((:textDocument/switchSourceHeader . ,(concat "file://" root "/a.h")))
        (my/lsp-find-other-file)
        (should (equal (mapcar #'car requests) '(:textDocument/switchSourceHeader)))
        (should (equal visited (list (expand-file-name "a.h" root))))
        (should-not ff-calls))
      (setq visited nil)
      ;; 対が無い (null)、要求失敗、非 LSP、C-u はいずれも ff (新規作成なし)
      (my-test-cpp-config--with-lsp-stubs t
          '((:textDocument/switchSourceHeader . nil))
        (my/lsp-find-other-file)
        (should (= (length requests) 1)))
      (my-test-cpp-config--with-lsp-stubs t
          `((:textDocument/switchSourceHeader
             . ,(my-test-cpp-config--lsp-error -32601 "method not found")))
        (my/lsp-find-other-file)
        (should (= (length requests) 1)))
      (my-test-cpp-config--with-lsp-stubs nil nil
        (my/lsp-find-other-file)
        (should-not requests))
      (my-test-cpp-config--with-lsp-stubs t nil
        (my/lsp-find-other-file '(4))
        (should-not requests))
      (should-not visited)
      (should (equal ff-calls '(nil nil nil nil))))))

(ert-deftest my-test-cpp-config-lsp-workspace-symbol ()
  "横断シンボル検索は LSP なら workspace/symbol を自前で送り、無ければ GTAGS、それも無ければエラー."
  :tags '(:cpp-config)
  (let (global-calls)
    (cl-letf (((symbol-function 'my/gtags--run)
               (lambda (flag pattern) (push (list flag pattern) global-calls) nil))
              ((symbol-function 'ggtags-find-project) (lambda () t)))
      ;; LSP: eglot の xref バックエンド (タイムアウト無期限) ではなく有限タイムアウトの要求
      (my-test-cpp-config--with-lsp-stubs t
          `((:workspace/symbol
             . [(:name "callee" :kind 12
                 :location (:uri ,uri :range (:start (:line 0 :character 4)
                                              :end (:line 0 :character 10))))
                ;; range の無い要素 (WorkspaceSymbol の簡略形) は読み飛ばす
                (:name "nowhere" :kind 12 :location (:uri ,uri))]))
        (my/lsp-find-workspace-symbol "callee")
        (should-not global-calls)
        (should (equal requests `((:workspace/symbol (:query "callee")
                                                     ,jsonrpc-default-request-timeout))))
        (should (= (length shown) 1))
        (should (string-match-p "int callee" (xref-item-summary (car shown))))
        (should (= (xref-location-line (xref-item-location (car shown))) 1))
        (should (zerop (hash-table-count eglot--temp-location-buffers))))
      (my-test-cpp-config--with-lsp-stubs nil nil
        (my/lsp-find-workspace-symbol "foo.*bar")
        (should (equal global-calls '(("--grep" "foo.*bar"))))
        (should-not requests))
      (setq global-calls nil)
      (my-test-cpp-config--with-lsp-stubs nil nil
        (should-error (my/lsp-find-workspace-symbol " ") :type 'user-error)
        (should-not global-calls)))
    (cl-letf (((symbol-function 'my/gtags--run)
               (lambda (flag pattern) (push (list flag pattern) global-calls) nil))
              ((symbol-function 'ggtags-find-project) (lambda () nil)))
      (my-test-cpp-config--with-lsp-stubs nil nil
        (should-error (my/lsp-find-workspace-symbol "alloc") :type 'user-error)
        (should-not global-calls)))))

(ert-deftest my-test-cpp-config-lsp-workspace-symbol-real-gtags-regexp ()
  "非 eglot の横断検索は Global の正規表現を保持し、xref と 0 件表示を返す."
  :tags '(:cpp-config :gtags-integration)
  (skip-unless (and (executable-find "global") (executable-find "gtags")))
  (let ((root (make-temp-file "my-test-lsp-global-regexp-" t))
        (process-environment (copy-sequence process-environment))
        (real-call (symbol-function 'my/gtags--call))
        shown message-text global-args)
    (unwind-protect
        (let* ((default-directory (file-name-as-directory root))
               (source (expand-file-name "sample.c" root)))
          (dolist (name '("GTAGSROOT" "GTAGSDBPATH" "GTAGSCONF" "GTAGSLABEL"))
            (setenv name nil))
          (with-temp-file source
            (insert "int foo12bar(void) { return 1; }\n"
                    "int fooXXbar(void) { return 2; }\n"
                    "/* fooABbar */\n"
                    "const char *text = \"fooZZbar\";\n"))
          (should (zerop (call-process "gtags" nil nil nil)))
          (cl-letf (((symbol-function 'my/gtags--lsp-p) (lambda () nil))
                    ((symbol-function 'my/gtags--call)
                     (lambda (&rest args)
                       (push args global-args)
                       (apply real-call args)))
                    ((symbol-function 'xref-show-xrefs)
                     (lambda (fetcher _action) (setq shown (funcall fetcher))))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-text (apply #'format format-string args)))))
            (my/lsp-find-workspace-symbol "foo.*bar")
            (should (= (length shown) 4))
            (should (equal (mapcar (lambda (item)
                                     (xref-location-line (xref-item-location item)))
                                   shown)
                           '(1 2 3 4)))
            (should (seq-every-p
                     (lambda (item)
                       (equal (xref-location-group (xref-item-location item)) source))
                     shown))
            (should (equal (xref-item-summary (car shown))
                           "int foo12bar(void) { return 1; }"))
            (setq shown nil)
            (my/lsp-find-workspace-symbol "^int foo[[:digit:]]*bar.*}$")
            (should (= (length shown) 1))
            (should (= (xref-location-line (xref-item-location (car shown))) 1))
            (setq shown nil)
            (my/lsp-find-workspace-symbol "foo12bar")
            (should (= (length shown) 1))
            (setq shown nil message-text nil)
            (my/lsp-find-workspace-symbol "no_such_symbol_22")
            (should-not shown)
            (should (equal message-text "シンボル no_such_symbol_22 は見つかりません"))
            (should (equal (nreverse global-args)
                           (mapcar (lambda (pattern)
                                     (list "--result=grep" "--encode-path=:%"
                                           "--grep" "--" pattern))
                                   '("foo.*bar" "^int foo[[:digit:]]*bar.*}$"
                                     "foo12bar" "no_such_symbol_22"))))))
      (delete-directory root t))))

;;; 実 clangd との結合
;; clangd がある環境でだけ、要求層をスタブせずに実 JSON-RPC で LSP 経路を通す
;; （PATH に clangd が無い場合の skip は tests/my-test-cpp-lane.el が許容する）。
(defun my-test-cpp-config--wait (pred secs)
  "PRED が真になるまで最大 SECS 秒待ち、最後の評価値を返す."
  (let ((deadline (+ (float-time) secs)))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.2))
    (funcall pred)))

(defun my-test-cpp-config--inlay-overlay-p ()
  "現在のバッファに eglot の inlay hint の overlay があれば non-nil."
  (seq-some (lambda (overlay) (overlay-get overlay 'eglot--inlay-hint))
            (overlays-in (point-min) (point-max))))

(ert-deftest my-test-cpp-config-lsp-live-clangd ()
  "実 clangd で呼び出し元・型階層・ヘッダ切替・横断検索・inlay hints・breadcrumb を通す."
  :tags '(:cpp-config)
  (skip-unless (executable-find "clangd"))
  (require 'eglot)
  (require 'json)
  (let* ((root (make-temp-file "my-test-clangd-" t))
         (cpp (expand-file-name "main.cpp" root))
         (hdr (expand-file-name "main.h" root))
         shown visited buffer server)
    (unwind-protect
        (progn
          (with-temp-file hdr (insert "int callee(int x);\n"))
          (with-temp-file cpp
            (insert "#include \"main.h\"\n"
                    "struct Base { virtual int value() const { return 1; } };\n"
                    "struct Derived : Base { int value() const override { return 2; } };\n"
                    "int callee(int x) { return x + 1; }\n"
                    "int caller(int y) { return callee(y) + callee(y); }\n"
                    "int main() { Derived d; return caller(d.value()); }\n"))
          (with-temp-file (expand-file-name "compile_commands.json" root)
            (insert (json-encode
                     `[((directory . ,root)
                        (command . ,(format "clang++ -std=c++17 -c %s" cpp))
                        (file . ,cpp))])))
          (setq buffer (find-file-noselect cpp))
          (with-current-buffer buffer
            (apply #'eglot (eglot--guess-contact))
            (should (my-test-cpp-config--wait #'eglot-managed-p 30))
            (setq server (eglot-current-server))
            (cl-letf (((symbol-function 'xref-show-xrefs)
                       (lambda (fetcher _action) (setq shown (funcall fetcher))))
                      ((symbol-function 'find-file)
                       (lambda (file &rest _) (push file visited))))
              ;; 呼び出し元: index が揃うまで再試行し、呼び出し箇所ごとに 1 件
              (goto-char (point-min))
              (search-forward "int callee(int x) {")
              (search-backward "callee")
              (should (my-test-cpp-config--wait
                       (lambda () (setq shown nil) (my/lsp-find-callers) shown) 20))
              (should (= (length shown) 2))
              (dolist (item shown)
                (should (string-prefix-p "caller:" (substring-no-properties (xref-item-summary item))))
                (should (= (xref-location-line (xref-item-location item)) 5)))
              ;; 呼び出し先: clangd 20 以降なら結果、それより前は版を示すエラー
              (goto-char (point-min))
              (search-forward "int caller(int y)")
              (search-backward "caller")
              (setq shown nil)
              (condition-case err
                  (progn (my/lsp-find-callees) (should shown))
                (user-error (should (string-match-p "clangd 20" (cadr err)))))
              ;; 型階層
              (goto-char (point-min))
              (search-forward "struct Derived")
              (search-backward "Derived")
              (setq shown nil)
              (my/lsp-find-type-hierarchy)
              (should (equal (mapcar (lambda (item)
                                       (substring-no-properties (xref-item-summary item) 0 4))
                                     shown)
                             '("[基底]")))
              ;; ヘッダ/ソース切替
              (setq visited nil)
              (my/lsp-find-other-file)
              (should (equal visited (list hdr)))
              ;; 横断シンボル検索
              (setq shown nil)
              (my/lsp-find-workspace-symbol "callee")
              (should (seq-some (lambda (item)
                                  (string-match-p "int callee" (xref-item-summary item)))
                                shown))
              ;; inlay hints: ON で overlay が付き、OFF で消える
              (my/eglot-inlay-hints-toggle)
              (should eglot-inlay-hints-mode)
              (jit-lock-fontify-now (point-min) (point-max))
              (should (my-test-cpp-config--wait #'my-test-cpp-config--inlay-overlay-p 10))
              (my/eglot-inlay-hints-toggle)
              (should-not eglot-inlay-hints-mode)
              (should-not (my-test-cpp-config--inlay-overlay-p))
              ;; breadcrumb: eglot-imenu の範囲付き index で関数名が出る
              (goto-char (point-min))
              (search-forward "return callee(y)")
              (require 'breadcrumb)
              (let ((imenu-auto-rescan t))
                (imenu--make-index-alist t))
              (should (string-match-p "caller"
                                      (substring-no-properties (breadcrumb-imenu-crumbs)))))))
      (when server
        (ignore-errors (eglot-shutdown server nil nil t)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest my-test-cpp-config-clangd-args ()
  "clangd の起動引数と xref 追従の設定を固定する."
  :tags '(:cpp-config)
  (let ((entry (assoc '(c-mode c-ts-mode c++-mode c++-ts-mode) eglot-server-programs)))
    (should entry)
    (should (equal (cadr entry) "clangd"))
    (should (member "--header-insertion=never" (cdr entry)))
    (should (member "--limit-references=0" (cdr entry)))
    ;; --limit-results は補完候補数の上限も兼ねるため触らない
    (should-not (seq-some (lambda (arg) (string-prefix-p "--limit-results" arg))
                          (cdr entry))))
  (should eglot-extend-to-xref))

;;; 読解補助 (outline)
;; outline-minor-mode は treesit 連動の見出しが取れる ts モードだけで有効化する。
;; cc-mode の既定 (全行が見出し) では実用にならないため、4 モード parity の意図的な例外。
(ert-deftest my-test-cpp-config-outline-hook-ts-only ()
  :tags '(:cpp-config)
  (dolist (hook '(c-ts-mode-hook c++-ts-mode-hook))
    (should (memq 'outline-minor-mode (default-value hook))))
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (should-not (and (boundp hook) (memq 'outline-minor-mode (default-value hook))))))

;; breadcrumb (header-line のパス表示) は cc-mode でも imenu で動くため 4 モードすべて
(ert-deftest my-test-cpp-config-breadcrumb-hook-parity ()
  :tags '(:cpp-config)
  (dolist (hook '(c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook))
    (should (memq 'breadcrumb-local-mode (default-value hook)))))

(ert-deftest my-test-cpp-config-outline-dwim-fallback ()
  "outline が無効なバッファでは <f9> が従来どおり hs-toggle-hiding を呼ぶ."
  :tags '(:cpp-config)
  (let (called)
    (cl-letf (((symbol-function 'hs-toggle-hiding)
               (lambda (&rest _) (interactive) (setq called t))))
      (with-temp-buffer
        (fundamental-mode)
        (should-not (bound-and-true-p outline-minor-mode))
        (call-interactively #'my/outline-cycle-dwim)
        (should called)))))

(ert-deftest my-test-cpp-config-outline-dwim-ts ()
  "ts モードでは関数定義が見出しになり、<f9> が本体の中からでも関数を折りたたむ."
  :tags '(:cpp-config)
  (skip-unless (my/treesit-cc-grammar-ready-p 'c))
  (let ((file (make-temp-file "my-test-outline-" nil ".c"))
        buffer)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#include <stdio.h>\n\nint one(void) {\n    return 1;\n}\n\n"
                    "int two(void) {\n    return 2;\n}\n"))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (should (eq major-mode 'c-ts-mode))
            (should outline-minor-mode)
            (should (eq outline-search-function #'treesit-outline-search))
            ;; 既定の述語は namespace / class も見出しにするため、関数定義ノード全体に
            ;; 一致する述語へ差し替え、定義行で looking-at 判定が通ること
            (should (eq treesit-outline-predicate #'my/c-ts-outline-predicate))
            (goto-char (point-min))
            (forward-line 2)
            (should (outline-on-heading-p))
            (goto-char (point-min))
            (should-not (outline-on-heading-p))
            (let ((body-1 (progn (goto-char (point-min))
                                 (search-forward "return 1") (match-beginning 0)))
                  (body-2 (progn (goto-char (point-min))
                                 (search-forward "return 2") (match-beginning 0))))
              ;; 本体の中から: 見出し (定義行) へ移動して折りたたむ
              (goto-char body-1)
              (call-interactively #'my/outline-cycle-dwim)
              (should (= (line-number-at-pos) 3))
              (should (invisible-p body-1))
              (should-not (invisible-p body-2))
              ;; もう一度で展開
              (call-interactively #'my/outline-cycle-dwim)
              (should-not (invisible-p body-1))
              ;; C-u でバッファ全体
              (let ((current-prefix-arg '(4)))
                (call-interactively #'my/outline-cycle-dwim))
              (should (invisible-p body-1))
              (should (invisible-p body-2))
              ;; 見出しより前 (#include 行) では失敗する
              (goto-char (point-min))
              (should-error (call-interactively #'my/outline-cycle-dwim)
                            :type 'user-error))))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (delete-file file))))

;;; 読解補助 (inlay hints)
;; 既定 OFF のまま、C-t l でバッファ単位に ON/OFF できること。eglot 本体の
;; eglot-inlay-hints-mode を通し、サーバーの capability 判定だけをスタブする。
(ert-deftest my-test-cpp-config-inlay-hints-default-off ()
  :tags '(:cpp-config)
  (should (memq :inlayHintProvider (default-value 'eglot-ignored-server-capabilities))))

(ert-deftest my-test-cpp-config-inlay-hints-toggle-requires-eglot ()
  :tags '(:cpp-config)
  (with-temp-buffer
    (should-error (my/eglot-inlay-hints-toggle) :type 'user-error)
    (should-not (local-variable-p 'eglot-ignored-server-capabilities))))

(ert-deftest my-test-cpp-config-inlay-hints-toggle-roundtrip ()
  "ON でローカルに :inlayHintProvider を外し、OFF でローカル値を捨てて既定へ戻す."
  :tags '(:cpp-config)
  (let ((default (default-value 'eglot-ignored-server-capabilities)))
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
              ;; eglot-server-capable の実装と同じく ignored を memq で見る
              ((symbol-function 'eglot-server-capable)
               (lambda (&rest feats)
                 (not (seq-some (lambda (feat) (memq feat eglot-ignored-server-capabilities))
                                feats)))))
      (with-temp-buffer
        (my/eglot-inlay-hints-toggle)
        (should eglot-inlay-hints-mode)
        (should (local-variable-p 'eglot-ignored-server-capabilities))
        (should-not (memq :inlayHintProvider eglot-ignored-server-capabilities))
        (should (memq :documentHighlightProvider eglot-ignored-server-capabilities))
        (should (equal (default-value 'eglot-ignored-server-capabilities) default))
        (my/eglot-inlay-hints-toggle)
        (should-not eglot-inlay-hints-mode)
        (should-not (local-variable-p 'eglot-ignored-server-capabilities))))
    ;; 非対応サーバーではモードが自分を切るため、エラーにしてローカル値も残さない
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
              ((symbol-function 'eglot-server-capable) (lambda (&rest _) nil)))
      (with-temp-buffer
        (should-error (my/eglot-inlay-hints-toggle) :type 'user-error)
        (should-not eglot-inlay-hints-mode)
        (should-not (local-variable-p 'eglot-ignored-server-capabilities))))
    (should (equal (default-value 'eglot-ignored-server-capabilities) default))))

(provide 'my-test-cpp-config)
;;; my-test-cpp-config.el ends here
