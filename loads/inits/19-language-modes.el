;;; 19-language-modes.el --- 各種モード設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; 主要なメジャーモードの設定を行う

;;; Code:

;;;;;; [Group] Code Folding - コード折りたたみ ;;;;;;
(use-package hideshow
  :straight nil
  :bind (("C-\\" . hs-toggle-hiding)
         ("<f5>" . hs-toggle-hiding))
  ;; ts モードは c-mode/c++-mode のフックを継承しないため個別に登録する
  ;; (hs-special-modes-alist は c-ts-mode / c++-ts-mode のエントリを持つ)
  :hook ((c-mode          . hs-minor-mode)
         (c++-mode        . hs-minor-mode)
         (c-ts-mode       . hs-minor-mode)
         (c++-ts-mode     . hs-minor-mode)
         (emacs-lisp-mode . hs-minor-mode)
         (lisp-mode       . hs-minor-mode))
  )

;;;;;; [Group] Programming Modes - プログラミングモード ;;;;;;
;;; Elisp-mode - elisp-mode の設定
(use-package elisp-mode
  :straight nil
  :hook (emacs-lisp-mode . my/emacs-lisp-mode-setup)
  :config
  (defun my/emacs-lisp-mode-setup ()
    "Emacs Lisp モード用の設定。"
    (setq indent-tabs-mode nil)  ; タブではなくスペースを使用
    (subword-mode 1))            ; CamelCase も単語として移動
  )

;;; Cc-mode - cc-mode の設定
(use-package cc-mode
  :straight nil
  :mode (("\\.C\\'"    . c-mode)
         ("\\.cc\\'"   . c++-mode)
         ("\\.nut\\'"  . c++-mode)
         ("\\.cpp\\'"  . c++-mode)
         ("\\.hh\\'"   . c++-mode)
         ("\\.c\\'"    . c-mode)
         ("\\.h\\'"    . c++-mode)
         ("\\.hpp\\'"  . c++-mode)
         ("\\.log\\'"  . c-mode)
         ("\\.cfg\\'"  . c-mode))
  :hook (c-mode-common . my/cc-mode-setup)
  :config
  (defun my/cc-mode-setup ()
    "C/C++ モード共通の設定。"
    (local-set-key (kbd "C-c c") 'compile) ; コンパイル
    (c-toggle-auto-hungry-state 1)         ; 自動改行 & 連続スペース一括削除
    (setq indent-tabs-mode nil
          c-basic-offset 4))
  )

;;; C-ts-mode - tree-sitter 版 C/C++ モード (段階移行)
;; 文法が導入済みなら c-ts-mode / c++-ts-mode、無ければ従来の cc-mode を使う。
;; 文法をビルドできない環境では何も起きず cc-mode のまま動作する（フォールバック）。
;; 文法の導入は 18-built-in-package.el の M-x my/treesit-install-c-grammars で行う。
;;
;; ts モードには c-toggle-auto-hungry-state が無いため、自動改行と連続スペース
;; 一括削除は組み込み機能で再現する（my/c-ts-mode-setup を参照）。
;; 波括弧が閉じていない入力途中は構文木が ERROR になるため、桁は括弧の深さから
;; 算出する（my/c-ts-error-context-p を参照）。構文が揃えば、ブレース初期化の
;; `{' 配置（意図的な差分。docs/cpp.md を参照）を除いて cc-mode + google-c-style
;; と一致する。
(defvar my/use-treesit-for-cc t
  "Non-nil なら文法が揃っている C/C++ で ts モードを使う。
nil にすると文法があっても cc-mode を使い続ける。")

(defconst my/cc-non-source-regexp "\\.\\(log\\|cfg\\|nut\\)\\'"
  "cc-mode を流用しているだけで C/C++ ではないファイル。
.log/.cfg はログ閲覧用の流用、.nut は Squirrel である。ts モードへ回すと
バッファ全体が ERROR ノードになりフォントロックを失うため remap 対象から除外する。")

(defun my/cc-real-source-p ()
  "現在のバッファが ts モードへ回してよい実 C/C++ ソースなら non-nil."
  (and buffer-file-name
       (not (string-match-p my/cc-non-source-regexp buffer-file-name))))

(defun my/treesit-cc-grammar-ready-p (lang)
  "LANG の文法が使える状態なら non-nil.
treesit-language-available-p は treesit.el をロードせず警告も出さない."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-available-p lang)))

(defun my/treesit-cc-ready-p (lang)
  "LANG で ts モードを使ってよいなら non-nil."
  (and my/use-treesit-for-cc (my/treesit-cc-grammar-ready-p lang)))

;; auto-mode から呼ばれる振り分け関数（組み込みの c-or-c++-mode と同じ形）。
;; 可用性はここで毎回判定するため、`my/use-treesit-for-cc' を後から nil にしても効く。
(defun my/c-mode-dispatch ()
  "実 C ソースなら `c-ts-mode'、流用ファイルなら `c-mode' を有効にする."
  (interactive)
  (if (and (my/treesit-cc-ready-p 'c) (my/cc-real-source-p))
      (c-ts-mode)
    (c-mode)))

(defun my/c++-mode-dispatch ()
  "実 C++ ソースなら `c++-ts-mode'、流用ファイルなら `c++-mode' を有効にする."
  (interactive)
  (if (and (my/treesit-cc-ready-p 'cpp) (my/cc-real-source-p))
      (c++-ts-mode)
    (c++-mode)))

(defconst my/treesit-cc-remap-entries
  '((c-mode   c   my/c-mode-dispatch)
    (c++-mode cpp my/c++-mode-dispatch))
  "(cc-mode 側のモード 文法 振り分け関数) の対応。")

(defun my/treesit-cc-apply-remap ()
  "文法が使える言語だけ `major-mode-remap-alist' へ振り分け関数を登録する.
C と C++ は独立に判定する（片方の文法だけある環境でも壊さない）。
`my/use-treesit-for-cc' が nil でも登録する。c-ts-mode.el はロード時に
`major-mode-remap-defaults' を書き換えるため、登録しておかないと off が
迂回されるうえ、.log/.cfg のような流用ファイルまで ts へ回ってしまう。
`major-mode-remap-alist' は defaults より優先されるため、ここでの指定が常に勝つ。"
  (pcase-dolist (`(,mode ,lang ,dispatch) my/treesit-cc-remap-entries)
    (if (my/treesit-cc-grammar-ready-p lang)
        (setf (alist-get mode major-mode-remap-alist) dispatch)
      (setq major-mode-remap-alist
            (assq-delete-all mode major-mode-remap-alist)))))

(my/treesit-cc-apply-remap)

(use-package c-ts-mode
  :straight nil
  ;; 起動経路で require しない（文法不在時に treesit の警告が出るため）
  :defer t
  ;; use-package はバイトコンパイル時にパッケージを先読みする。c-ts-mode.el は
  ;; ロード時に treesit-ready-p を呼ぶため、先読みすると make lint が警告で汚れる。
  :no-require t
  :hook ((c-ts-mode   . my/c-ts-mode-setup)
         (c++-ts-mode . my/c-ts-mode-setup))
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style #'my/c-ts-mode-indent-style)
  ;; :hook から参照する関数は :preface で定義する (:init だと多重定義警告が出る)
  :preface
  ;; --- c-toggle-auto-hungry-state 相当 ---------------------------------
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

  ;; --- 自動改行の抑止条件（cc-mode の c-hanging-semi&comma-criteria 相当）------
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

  ;; --- cleanup（cc-mode の c-cleanup-list 相当）--------------------------------
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

  (defun my/c-ts-mode-setup ()
    "C/C++ ts モード共通の設定（cc-mode 側の `my/cc-mode-setup' と対応）."
    (local-set-key (kbd "C-c c") 'compile)          ; コンパイル
    (local-set-key (kbd "DEL") 'my/c-ts-hungry-delete-backward) ; hungry delete 相当
    (setq-local electric-layout-rules my/c-ts-electric-layout-rules)
    ;; cleanup は electric-layout (深さ 40) より先に走らせる
    (add-hook 'post-self-insert-hook #'my/c-ts-pre-layout-fixups -10 t)
    (electric-layout-local-mode 1)                  ; auto-newline 相当
    (setq indent-tabs-mode nil))

  ;; --- 入力途中（ERROR 状態）のインデント -------------------------------------
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
    (let* ((offset (if (boundp 'c-ts-mode-indent-offset) c-ts-mode-indent-offset 4))
           (depth (seq-count (lambda (pos) (not (my/c-ts--non-indenting-open-p pos)))
                             (nth 9 (my/c-ts--ppss bol))))
           (closing (save-excursion (goto-char bol) (looking-at-p "[ \t]*[)}]"))))
      (* offset (max 0 (if closing (1- depth) depth)))))

  (defun my/c-ts-mode-indent-style ()
    "k&r をベースに google-c-style 相当の差分を前置した indent 規則を返す.
前置した規則が先に照合されるためベース側の同種規則を上書きできる。
ベース取得に使う `c-ts-mode--indent-styles' は内部関数のため、
失われた場合でも差分規則だけで動作を継続する（エラーにしない）。"
    (let* ((offset (if (boundp 'c-ts-mode-indent-offset) c-ts-mode-indent-offset 4))
           (half (/ offset 2))
           (base (when (fboundp 'c-ts-mode--indent-styles)
                   (alist-get 'k&r (c-ts-mode--indent-styles
                                    (if (derived-mode-p 'c++-ts-mode) 'cpp 'c))))))
      (append
       `(;; google-c-style の (access-label . /): public: 等をメンバより半段浅く置く。
         ;; ERROR 状態でも access_specifier は照合できるため ERROR 規則より前へ置く
         ;; （順序を逆にすると入力途中だけ 1 段深くなる）。
         ((node-is "access_specifier") parent-bol ,half)
         ;; 入力途中で構文木が壊れている間の桁（`my/c-ts-error-context-p' を参照）
         (my/c-ts-error-context-p column-0 my/c-ts-error-offset)
         ;; google-c-style の (innamespace . 0): namespace 本体をインデントしない
         ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)
         ;; google-c-style の (case-label . +): case を switch から 1 段下げる
         ((node-is "case_statement") standalone-parent ,offset))
       base)))
  )

;;;;;; [Group] Text Editing - テキスト編集 ;;;;;;
(use-package text-mode
  :straight nil
  :mode (("\\.txt\\'" . text-mode)
         ("\\.tmp\\'" . text-mode))
  :hook (text-mode . my/text-mode-setup)
  :config
  (defun my/text-mode-setup ()
    "テキストモード用の設定。ただし markdown-mode では適用しない。"
    (unless (derived-mode-p 'markdown-mode) ;; markdown-mode では適用しない
      (setq indent-tabs-mode nil
            tab-width 2)
      (subword-mode 1))) ; CamelCase も単語として移動
  )

(provide '19-language-modes)
;;; 19-language-modes.el ends here
