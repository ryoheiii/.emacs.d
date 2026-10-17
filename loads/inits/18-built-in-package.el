;;; 18-built-in-package.el --- 組み込みパッケージの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; 表示・保存・検索などの組み込み機能と、C/C++ の言語サーバーを設定する。

;;; Code:

;; 遅延ロード先とプラットフォーム固有定義をコンパイラへ伝える。
(declare-function eglot-managed-p "eglot")
(declare-function eglot-inlay-hints-mode "eglot")
(defvar eglot-inlay-hints-mode)
(defvar eglot-ignored-server-capabilities)
(defvar my/auto-save-visited-in-progress)

;;;;; [Group] 差分・比較 ;;;;;
(use-package ediff
  :straight nil
  :defer t ; 組み込みコマンドの autoload で初回利用時に読み込む
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain) ;; コントロールバッファを同一フレームに表示
  (ediff-split-window-function 'split-window-horizontally)) ;; diff のバッファを左右に配置

;;;;; [Group] 視覚的な補助 ;;;;;
;;; paren - 対応する括弧を強調表示
(use-package paren
  :straight nil
  ;; show-paren-mode は Emacs 28+ でデフォルト有効のため hook 不要
  :custom
  (show-paren-delay 0)      ;; 遅延なしで即時ハイライト
  (show-paren-style 'mixed) ;; 対応する括弧が見えないときは、括弧内の式も強調する
  :custom-face
  (show-paren-match ((t (:background "#6b93b7" :foreground "#2e3440" :weight bold))))) ;; 薄めの青系

;;; Whitespace - 空白文字の視覚化。スペース、タブ、行末の空白などを明確に表示
(use-package whitespace
  :straight nil
  :hook (after-init . global-whitespace-mode)
  :custom
  (whitespace-style '(face
                      trailing   ; 行末
                      tabs
                      spaces
                      empty      ; 先頭/末尾の空行
                      space-mark ; 表示のマッピング
                      tab-mark
                      newline))
  (whitespace-display-mappings '((space-mark ?\x3000 [?\u25a1]) (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (whitespace-space-regexp "\\(\u3000+\\)") ; スペースは全角のみを可視化
  (whitespace-trailing-regexp "\\([ \u00A0]+\\)$")
  :config
  (set-face-attribute 'whitespace-trailing nil :background "Black" :foreground "DeepPink" :underline t)
  (set-face-attribute 'whitespace-tab nil :background "Black" :foreground "DarkMagenta" :underline t)
  (set-face-attribute 'whitespace-space nil :background "Black" :foreground "GreenYellow" :weight 'bold)
  (set-face-attribute 'whitespace-empty nil :background "Black"))

;;; display-fill-column-indicator - fill-column の桁に目安の線を表示
(use-package display-fill-column-indicator
  :straight nil
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (markdown-mode . display-fill-column-indicator-mode))
  :custom
  (display-fill-column-indicator-column 120)
  (display-fill-column-indicator-character ?|))

;;; 表示設定
(use-package time
  :straight nil
  :hook (after-init . display-time-mode)
  :custom
  (display-time-day-and-date t)
  (display-time-string-forms '((format "%s/%s (%s) %s:%s"
                                       month day dayname 24-hours minutes))))

;;;;; [Group] 構文解析基盤 ;;;;;
;;; Treesit - 文法ライブラリの配置先と導入経路
;; 配置先・取得元・導入コマンドは loads/site-elisp/my-treesit.el が持つ。
;; use-package と straight に依存しないライブラリにしてあり、emacs-setup.sh の
;; --setup-treesit が同じファイルを単体ロードして文法をビルドする。
;;
;; 注意: 起動経路で c-ts-mode を require してはならない。ロードすると treesit.el まで
;; 読み込まれ起動が重くなる (遅延は my-test-packages-deferred-features が固定する)。
;; 可用性判定には treesit.el をロードせず警告も出さない treesit-language-available-p を
;; 使う (treesit-ready-p は文法不在時に警告を出す)。
(use-package treesit
  :straight nil
  ;; treesit.el のロードは grammar 導入時だけで足りる (起動経路では読まない)。
  ;; 初期化では導入コマンドだけを用意し、構文解析の本体は必要になってから読む。
  :defer t
  :init
  (require 'my-treesit))

;;;;; [Group] 言語サーバー ;;;;;
;; C/C++ 補完の三段フォールバック (環境に応じて自動選択)
;;   1. clangd + compile_commands.json/.clangd あり → eglot
;;   2. irony-server 実体あり                        → irony (31-editing.el)
;;   3. どちらも無い                                  → cape + ggtags
(use-package eglot
  :straight nil
  ;; ts モードは c-mode/c++-mode のフックを継承しないため個別に登録する
  :hook ((c-mode      . my/eglot-cc-maybe-ensure)
         (c++-mode    . my/eglot-cc-maybe-ensure)
         (c-ts-mode   . my/eglot-cc-maybe-ensure)
         (c++-ts-mode . my/eglot-cc-maybe-ensure)
         (eglot-managed-mode . my/eglot-cc-suppress-irony))
  ;; :hook から参照する関数は :preface で定義する (:init だと多重定義警告が出る)
  :preface
  (defconst my/eglot-cc-file-regexp "\\.\\(c\\|cc\\|C\\|cpp\\|cxx\\|h\\|hh\\|hpp\\|hxx\\)\\'"
    "eglot を自動起動してよい C/C++ 実ソースの拡張子。.log/.cfg (c-mode 割当) を除外する.")
  (defun my/eglot-cc-project-p ()
    "compile_commands.json または .clangd を持つプロジェクトなら non-nil."
    (and buffer-file-name
         (or (locate-dominating-file default-directory "compile_commands.json")
             (locate-dominating-file default-directory ".clangd")
             (locate-dominating-file
              default-directory
              (lambda (dir)
                (file-exists-p
                 (expand-file-name "build/compile_commands.json" dir)))))))
  (defun my/eglot-cc-maybe-ensure ()
    "clangd があり CDB または .clangd を持つ C/C++ 実ソースのみ eglot を自動起動する.
.clangd 単独 (CDB なし) のプロジェクトも意図的に自動起動対象とする."
    (when (and buffer-file-name
               (string-match-p my/eglot-cc-file-regexp buffer-file-name)
               (executable-find "clangd")
               (my/eglot-cc-project-p))
      (eglot-ensure)))
  (defun my/eglot-inlay-hints-toggle ()
    "現在のバッファで eglot の inlay hints (引数名・推論型のヒント) を切り替える.
既定は `eglot-ignored-server-capabilities' の :inlayHintProvider で常時 OFF。
ON にするときはバッファローカルに :inlayHintProvider を外してから
`eglot-inlay-hints-mode' を有効化する (`eglot-server-capable' は同変数を memq で
見るためローカル値が効く)。OFF ではローカル値を捨てて既定へ戻す。
ローカル値は ON の間だけ残るため、`eglot-reconnect' 後の自動有効化
(`eglot--maybe-activate-editing-mode') でも ON が維持される。
eglot 未管理のバッファでは `user-error'。"
    (interactive)
    (unless (and (fboundp 'eglot-managed-p) (eglot-managed-p))
      (user-error "eglot (clangd) が管理していないバッファです"))
    (if eglot-inlay-hints-mode
        (progn
          (eglot-inlay-hints-mode -1)
          (kill-local-variable 'eglot-ignored-server-capabilities)
          (message "inlay hints: off"))
      (setq-local eglot-ignored-server-capabilities
                  (remq :inlayHintProvider eglot-ignored-server-capabilities))
      (eglot-inlay-hints-mode 1)
      ;; 非対応サーバーではモードが自分を切る (eglot-inlay-hints-mode の定義を参照)
      (if eglot-inlay-hints-mode
          (message "inlay hints: on")
        (kill-local-variable 'eglot-ignored-server-capabilities)
        (user-error "サーバーが inlay hints に対応していません"))))
  ;; eglot 管理バッファでは irony を止める (CAPF 競合防止)。
  ;; eglot-ensure は post-command-hook で遅延接続するため、モードフックでの判定は不可
  (defun my/eglot-cc-suppress-irony ()
    (when (and (eglot-managed-p) (bound-and-true-p irony-mode))
      (irony-mode -1)))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format full))
  ;; inlay hints は既定 OFF。C-t l (my/eglot-inlay-hints-toggle) でバッファ単位に ON にできる
  (eglot-ignored-server-capabilities '(:inlayHintProvider :documentHighlightProvider))
  (eglot-stay-out-of '(flymake))       ; 使用感維持。診断が欲しくなったらここから外す
  ;; xref で飛んだプロジェクト外のファイル (/usr/include など) も同じサーバーの管理下に
  ;; 入れ、移動先でも C-t の LSP 経路を使えるようにする。別プロジェクトのファイルへ
  ;; 飛んだときは新しいサーバーを立てず元のサーバーを使う
  (eglot-extend-to-xref t)
  (eldoc-echo-area-use-multiline-p nil)
  :config
  ;; clangd 起動引数 (Doom :lang cc 準拠)。--header-insertion=never は補完確定時の
  ;; #include 自動挿入を止めるために必須 (使用感維持)。--clang-tidy は診断が増えるため不採用。
  ;; --limit-references=0 は参照検索 (C-t u / C-t c の LSP 経路) の既定上限 1000 件を外す。
  ;; --limit-results はコード補完の候補数上限 (100) も兼ねるため変えない
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode)
                 . ("clangd" "--background-index" "--header-insertion=never"
                    "--header-insertion-decorators=0" "--limit-references=0"))))

;;;;; [Group] 検索 ;;;;;
;;; grep - ファイルの内容を検索
;; consult-ripgrep (C-x g) が .gitignore を尊重するのに対し、C-c g は生 grep
(use-package grep
  :straight nil
  :bind ("C-c g" . grep)
  :custom
  ;; 初期入力を "grep -nr -e  ." とし、カーソルを -e の直後へ置く
  ;; (cons 形式は組み込み grep-default-command が string-match で型エラーになるため不可)
  (grep-command "grep -nr -e  .")
  (grep-command-position 13))

;;;;; [Group] 編集補助 ;;;;;
;; Elec-pair - 括弧の自動補完設定。入力中の括弧を自動的にペアで補完
(use-package elec-pair
  :straight nil
  :hook (after-init . electric-pair-mode))

;;; savehist - ミニバッファ入力と検索語の履歴を保存
(use-package savehist
  :straight nil
  :hook (after-init . savehist-mode)
  :custom
  (history-length 3000)
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-file (my-set-history "savehist")))

;;; saveplace - ファイルを開き直したときにカーソル位置を復元
(use-package saveplace
  :straight nil
  :hook (after-init . save-place-mode)
  :custom
  (save-place-file (my-set-history "places")))

;;; 他プロセスの編集をバッファに反映
(use-package autorevert
  :straight nil
  :hook (after-init . global-auto-revert-mode))

;;; auto-save-visited - アイドル時に訪問中のファイルへ自動保存
;; タイマーが呼ぶ保存関数を差し替え、00-core.el の保存フックがタイマー経由の保存を
;; 識別できるようにする（入力途中の末尾空白を消さない・モードラインを光らせない）。
;; auto-save-visited-mode は run-with-idle-timer に save-some-buffers と引数を直接渡すため、
;; 関数だけを入れ替えて引数（:no-prompt と述語）は維持する。
(use-package files
  :straight nil
  ;; :hook から参照する関数は :preface で定義する (:init だと多重定義警告が出る)
  :preface
  (defun my/auto-save-visited-save-some-buffers (&rest args)
    "`my/auto-save-visited-in-progress' を束縛して `save-some-buffers' を呼ぶ。"
    (let ((my/auto-save-visited-in-progress t))
      (apply #'save-some-buffers args)))
  (defun my/auto-save-visited-mark-timer ()
    "`auto-save-visited-mode' のタイマーの関数を識別可能なラッパーへ差し替える。
内部変数 `auto-save--timer' が無い Emacs では何もしない（従来どおりの保存になる）。"
    (when (and auto-save-visited-mode
               (bound-and-true-p auto-save--timer)
               (timerp auto-save--timer))
      (timer-set-function auto-save--timer
                          #'my/auto-save-visited-save-some-buffers
                          (timer--args auto-save--timer))))
  :custom (auto-save-visited-interval 30)
  :hook ((after-init . auto-save-visited-mode)
         (auto-save-visited-mode . my/auto-save-visited-mark-timer)))

;;; delsel - 入力した文字で選択範囲を置き換える
(use-package delsel
  :straight nil
  :hook (after-init . delete-selection-mode))

;;;;; [Group] ウィンドウ・バッファ管理 ;;;;;
;;; Windmove - ウィンドウ間の移動設定。Shift + 矢印キーでウィンドウ間を移動
(use-package windmove
  :straight nil
  :hook (after-init . windmove-default-keybindings))

;;; Uniquify - バッファ名のユニーク化。同名ファイルを開いた際にディレクトリ名で区別
(use-package uniquify
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-flag t)
  (uniquify-ignore-buffers-re "^\\*"))

;;; Tab-bar - Emacsのタブ機能をカスタマイズ
(use-package tab-bar
  :straight nil
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-tab-name-function 'tab-bar-tab-name-all)
  ;; C-z をタブ操作のプレフィックスキーにする
  :bind (:prefix-map tab-bar-prefix-map
         :prefix "C-z"
         ("n"   . tab-next)
         ("C-n" . tab-next)
         ("p"   . tab-previous)
         ("C-p" . tab-previous)
         ("f"   . tab-new)
         ("C-f" . tab-new)
         ("k"   . tab-close)
         ("C-k" . tab-close)
         ;; 番号のタブへ移動。引数なしの tab-bar-select-tab は押した数字キーを番号に使う
         ("1" . tab-bar-select-tab)
         ("2" . tab-bar-select-tab)
         ("3" . tab-bar-select-tab)
         ("4" . tab-bar-select-tab)
         ("5" . tab-bar-select-tab)
         ("6" . tab-bar-select-tab)
         ("7" . tab-bar-select-tab)
         ("8" . tab-bar-select-tab)
         ("9" . tab-bar-select-tab)))

;;;;; [Group] パフォーマンス最適化 ;;;;;
;;; so-long - 非常に長い行があるバッファで、表示の重い機能を抑える
(use-package so-long
  :straight nil
  :hook (after-init . global-so-long-mode))

;;; tramp - defer で遅延読み込み
(use-package tramp
  :straight nil
  :defer t)

;;; repeat - キーのリピート (multiple-cursors などで利用)
(use-package repeat
  :straight nil
  :hook (after-init . repeat-mode))

(provide '18-built-in-package)
;;; 18-built-in-package.el ends here
