;;; my-c-ts-edit.el --- C/C++ tree-sitter モードの編集処理 -*- lexical-binding: t; -*-
;;; Commentary:
;; 19-language-modes.el の :preface から読み込む。
;; cc-mode 相当の自動改行・空白削除と、入力途中のインデントを定義する。
;; モードの有効化やフックの登録は呼び出し元で行う。

;;; Code:

(require 'seq)

;; treesit 本体は先読みせず、コンパイラに参照先だけを伝える。
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-parent-until "treesit")
(defvar c-ts-indent-offset)
(defvar treesit-outline-predicate)
(defvar treesit-simple-indent-override-rules)

;;; 空白の一括削除
;; ts モードには cc-mode の auto-newline / hungry delete が無い。
;; cc-mode + google-c-style の実挙動に合わせて組み込み機能で再現する
;; （外部パッケージを増やさず tty でもそのまま動く）。
(defun my/c-ts-in-literal-p ()
  "point がコメントまたは文字列の中なら non-nil（cc-mode の `c-in-literal' 相当）."
  (nth 8 (syntax-ppss)))

(defun my/c-ts-hungry-delete-backward (&optional arg)
  "直前の空白をまとめて削除する（cc-mode の hungry delete 相当）.
改行も対象にする点まで `c-hungry-delete-backwards' に合わせる。
リージョン選択中は選択範囲を消す。ARG 付きとコメント・文字列の中では
cc-mode の `c-backspace-function' と同じ通常削除へ戻す（kill-ring は使わない）。"
  (interactive "P")
  (cond
   ((use-region-p) (delete-region (region-beginning) (region-end)))
   ((or arg (my/c-ts-in-literal-p))
    (backward-delete-char-untabify (prefix-numeric-value arg)))
   (t (let ((backward-delete-char-untabify-method 'all))
        (backward-delete-char-untabify 1)))))

;;; 自動改行の条件
;; electric-layout は挿入位置の文脈を見ないため、規則側で判定する。
(defun my/c-ts-before-nonblank-p ()
  "point の後ろに空白以外が残っているなら non-nil.
cc-mode の `c-semi&comma-no-newlines-before-nonblanks' 相当（行途中編集を壊さない）。"
  (not (looking-at-p "[ \t]*$")))

(defun my/c-ts-inside-parens-p ()
  "`(' の内側なら non-nil.
cc-mode の `c-semi&comma-inside-parenlist' 相当（for の区切りで改行しない）。"
  (let ((open (nth 1 (syntax-ppss))))
    (and open (eq (char-after open) ?\())))

(defun my/c-ts-layout-inhibit-p ()
  "リテラル内・行途中・前置引数付きでは自動改行しない.
前置引数付きの self-insert は cc-mode の電気コマンドも素通しにする
（`C-u 2 ;' は `;;' を入れるだけで改行しない）。"
  (or current-prefix-arg
      (my/c-ts-in-literal-p)
      (my/c-ts-before-nonblank-p)))

(defun my/c-ts-layout-open-brace ()
  "`{' の後ろで改行する."
  (unless (my/c-ts-layout-inhibit-p) 'after))

(defun my/c-ts-layout-close-brace ()
  "`}' の前後で改行する。空の `{}' 対なら後ろだけ改行する."
  (unless (my/c-ts-layout-inhibit-p)
    (if (eq (char-before (1- (point))) ?\{) 'after 'around)))

(defun my/c-ts-layout-semi ()
  "文末の `;' の後ろで改行する。`for' の区切りなど括弧の中では改行しない."
  (unless (or (my/c-ts-layout-inhibit-p) (my/c-ts-inside-parens-p))
    'after))

(defun my/c-ts-layout-colon ()
  "アクセス指定子の `:' でだけ改行する.
google-c-style の (access-label after) / (case-label) に合わせ、
case ラベル・三項演算子・スコープ解決演算子では改行しない。"
  (unless (my/c-ts-layout-inhibit-p)
    (save-excursion
      (beginning-of-line)
      (when (looking-at "[ \t]*\\(public\\|private\\|protected\\)[ \t]*:[ \t]*$")
        'after))))

;;; 自動改行後の整形
(defconst my/c-ts-close-brace-followers-regexp
  "\\`[ \t]*\\(;\\|,\\|else\\|while\\|catch\\)\\'"
  "`}' の直後の改行を取り消す語。
google-c-style の (defun-close-semi list-close-comma brace-else-brace
brace-elseif-brace brace-catch-brace) に対応する。
`;' と `,' は `}' へ直付けし、語の場合は空白 1 個を挟む。
`scope-operator' は `my/c-ts-pre-layout-fixups' の別枝で扱う。")

(defun my/c-ts--previous-code-char-p (char)
  "行頭のトークンの直前（空白と改行を飛ばす）が CHAR なら non-nil."
  (save-excursion
    (back-to-indentation)
    (skip-chars-backward " \t\n")
    (eq (char-before) char)))

(defun my/c-ts--join-to-previous (sep)
  "行頭のトークンを直前の非空白文字へ SEP で繋ぐ（間の空白と改行を畳む）."
  (let ((token (save-excursion (back-to-indentation) (point))))
    (save-excursion
      (goto-char token)
      (skip-chars-backward " \t\n")
      (delete-region (point) token)
      (insert sep))))

(defun my/c-ts-pre-layout-fixups ()
  "自動改行の直前に走らせる整形（cc-mode の `c-cleanup-list' 相当）.
`electric-layout'（深さ 40）と `electric-indent'（深さ 60）より先に走る必要がある。
point の後ろに空白以外が残る行では何もしない。cc-mode の cleanup も行末でだけ
働くため、既存行の途中や行頭へ挿入したときに前の行を巻き込まない。"
  (unless (or current-prefix-arg
              (my/c-ts-in-literal-p)
              (my/c-ts-before-nonblank-p))
    (let ((line (buffer-substring-no-properties (line-beginning-position) (point))))
      (cond
       ;; `}' の後ろへ入れた改行を、次行が `;' / `,' / else / while / catch なら取り消す
       ((and (string-match my/c-ts-close-brace-followers-regexp line)
             (my/c-ts--previous-code-char-p ?\}))
        (my/c-ts--join-to-previous
         (if (member (match-string 1 line) '(";" ",")) "" " ")))
       ;; scope-operator 相当: アクセス指定子の改行で割れた `::' を繋ぎ直す
       ;; （`public::X' のように `public' がマクロや名前空間の場合に起きる）
       ((and (string-match-p "\\`[ \t]*:\\'" line)
             (my/c-ts--previous-code-char-p ?:))
        (my/c-ts--join-to-previous ""))
       ;; empty-defun-braces 相当: `{' の直後の空行へ `}' を置いたら 1 行へ戻す
       ((string-match-p "\\`[ \t]*}\\'" line)
        (let ((brace (1- (point))))
          (save-excursion
            (goto-char brace)
            (skip-chars-backward " \t\n")
            (when (eq (char-before) ?\{)
              (delete-region (point) brace)))))))))

(defconst my/c-ts-electric-layout-rules
  '((?\{ . my/c-ts-layout-open-brace)
    (?\} . my/c-ts-layout-close-brace)
    (?\; . my/c-ts-layout-semi)
    (?\: . my/c-ts-layout-colon))
  "ts モードの自動改行規則。cc-mode + google-c-style の実挙動に対応させる。")

;;; モードへの適用と関数見出し

(defun my/c-ts-outline-predicate (node)
  "NODE が関数定義なら non-nil (outline の見出し判定).
c-ts-mode 既定の `c-ts-mode--outline-predicate' は function_declarator (名前と
引数) に加えて namespace_definition と class_specifier も見出しにする。
<f9> と consult-outline の見出しを関数定義だけに保つため、定義ノード全体に
一致させる (定義行で `outline-on-heading-p' が真になり、`treesit-outline-level'
も同じ結果になる)。"
  (equal (treesit-node-type node) "function_definition"))

(defun my/c-ts-mode-setup ()
  "C/C++ ts モード共通の設定（cc-mode 側の `my/cc-mode-setup' と対応）."
  ;; outline の見出しを関数定義ノード全体にする (理由は my/c-ts-outline-predicate)
  (setq-local treesit-outline-predicate #'my/c-ts-outline-predicate)
  ;; google-c-style 相当の差分は 31.1 の上書き規則へ置く。標準規則より先に評価され、
  ;; `c-ts-mode-set-style' で標準規則を作り直しても消えない。
  (let ((rules (my/c-ts-indent-rules)))
    (setq-local treesit-simple-indent-override-rules
                `((c ,@rules) (cpp ,@rules))))
  (keymap-local-set "C-c c" 'compile)             ; コンパイル
  (keymap-local-set "DEL" 'my/c-ts-hungry-delete-backward) ; hungry delete 相当
  (setq-local electric-layout-rules my/c-ts-electric-layout-rules)
  ;; cleanup は electric-layout (深さ 40) より先に走らせる
  (add-hook 'post-self-insert-hook #'my/c-ts-pre-layout-fixups -10 t)
  (electric-layout-local-mode 1)                  ; auto-newline 相当
  (setq indent-tabs-mode nil))

;;; 入力途中のインデント
;; 波括弧が 2 段以上開いていると、tree-sitter は木全体を ERROR へ落とす
;; （1 段なら MISSING "}" で復旧する）。既定の規則はこのとき桁 0 へ倒すため、
;; 改行するたびに行頭へ張り付く。括弧の深さから桁を算出して代替する。
;; 完成したコードでは ERROR が出ないため、通常のインデント結果へは影響しない。
(defun my/c-ts--ppss (pos)
  "POS の構文状態を返す.
`syntax-ppss' は `parse-partial-sexp' の副作用で point を POS へ残す。
インデント関数から素で呼ぶと以降の自己挿入が行頭へ入りバッファが壊れるため、
必ずこのラッパ経由で呼ぶ。"
  (save-excursion (syntax-ppss pos)))

(defun my/c-ts--backward-token ()
  "point の直前の識別子（`::' を含む）を返し、その先頭へ移動する.
呼び出し側で `save-excursion' すること。区切り文字の直後では空文字列を返す。"
  (skip-chars-backward " \t\n")
  (let ((end (point)))
    (skip-chars-backward "A-Za-z0-9_:")
    (buffer-substring-no-properties (point) end)))

(defun my/c-ts--non-indenting-open-p (pos)
  "POS の開き括弧をインデント段数へ数えないなら non-nil.
google-c-style の (innamespace . 0) に合わせ namespace 本体の波括弧だけ除外する
\(`extern \"C\"' は google-c-style が指定しておらず cc-mode 既定の + になる)。
行頭ではなく `{' の直前の語を見るため、`namespace ns { class C {' の内側や
`namespace alias = t; class C {' を namespace 本体と取り違えない。`{' を次行へ
置いた配置、無名 namespace、`namespace a::b {' にも対応する。"
  (and (eq (char-after pos) ?\{)
       (save-excursion
         (goto-char pos)
         (let ((name (my/c-ts--backward-token)))
           (or (equal name "namespace")               ; 無名 namespace
               (and (string-match-p "\\`[A-Za-z_][A-Za-z0-9_:]*\\'" name)
                    (equal (my/c-ts--backward-token) "namespace")))))))

(defun my/c-ts--toplevel-error-p (pos)
  "POS が root 直下の ERROR ノードに属するなら non-nil.
波括弧が 2 段以上開いた入力途中では、tree-sitter が末尾側をまとめて root 直下の
ERROR へ落とすため、既定の規則が桁 0 しか返せなくなる。関数本体の中だけが
壊れている場合（ERROR がより内側にある）は既定の規則が正しい桁を出せるので、
ここでは拾わない。

判定は POS そのものではなく直前のコードトークンで行う。コメントは ERROR の外側へ
付くことがあり、POS の直前がコメントだと壊れた木を見落とすため。"
  (let* ((probe (save-excursion
                  (goto-char pos)
                  (forward-comment (- (point-max)))
                  (max (point-min) (1- (point)))))
         (err (treesit-parent-until
               (treesit-node-at probe)
               (lambda (n) (equal (treesit-node-type n) "ERROR"))
               t)))
    (and err
         (let ((up (treesit-node-parent err)))
           (and up (null (treesit-node-parent up)))))))

(defun my/c-ts-error-context-p (_node parent bol)
  "入力途中の木の壊れで既定規則が桁を決められない文脈なら non-nil.
次の 2 つで発火する。

1. root 直下の ERROR が末尾を飲み込んでいる（`my/c-ts--toplevel-error-p'）。
   `treesit--indent-1' は BOL に開始位置を持つノードが無い場合（＝空行）、NODE へ
   nil、PARENT へ `treesit-node-on' の結果を渡す。この状態の PARENT は root に
   なるため (parent-is \"ERROR\") では捕まらない。
2. PARENT が ERROR そのもの（従来の (parent-is \"ERROR\") 相当）。

木の内側だけが壊れている場合は、既定の規則が正しい桁を出せるので譲る。
コメント・文字列の中と preproc 行もベース側の専用規則へ譲る。"
  (and (not (nth 8 (my/c-ts--ppss bol)))
       (not (save-excursion (goto-char bol) (looking-at-p "[ \t]*#")))
       (or (my/c-ts--toplevel-error-p bol)
           (equal (treesit-node-type parent) "ERROR"))))

(defun my/c-ts-error-offset (_node _parent bol)
  "BOL の括弧の深さからインデント桁を算出する（行頭が閉じ括弧なら 1 段戻す）."
  (let* ((offset (if (boundp 'c-ts-indent-offset) c-ts-indent-offset 4))
         (depth (seq-count (lambda (pos) (not (my/c-ts--non-indenting-open-p pos)))
                           (nth 9 (my/c-ts--ppss bol))))
         (closing (save-excursion (goto-char bol) (looking-at-p "[ \t]*[)}]"))))
    (* offset (max 0 (if closing (1- depth) depth)))))

(defun my/c-ts-indent-rules ()
  "標準の k&r より先に評価する google-c-style 相当の差分規則を返す.
モードフックで `treesit-simple-indent-override-rules' へ置く."
  (let* ((offset c-ts-indent-offset)
         (half (/ offset 2)))
    `(;; google-c-style の (access-label . /): public: 等をメンバより半段浅く置く。
      ;; ERROR 状態でも access_specifier は照合できるため ERROR 規則より前へ置く
      ;; （順序を逆にすると入力途中だけ 1 段深くなる）。
      ((node-is "access_specifier") parent-bol ,half)
      ;; 入力途中で構文木が壊れている間の桁（`my/c-ts-error-context-p' を参照）
      (my/c-ts-error-context-p column-0 my/c-ts-error-offset)
      ;; snapshot の直前アクセス指定子規則より、クラス・構造体の閉じ括弧を優先する。
      ((match "}" "field_declaration_list") parent-bol 0)
      ;; google-c-style の (innamespace . 0): namespace 本体をインデントしない
      ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)
      ;; google-c-style の (case-label . +): case を switch から 1 段下げる
      ((node-is "case_statement") standalone-parent ,offset))))

(provide 'my-c-ts-edit)
;;; my-c-ts-edit.el ends here
