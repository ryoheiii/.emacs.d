;;; 31-editing.el --- コード編集とタグナビゲーションの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; コード編集、C/C++ スタイル、タグナビゲーション、スニペット関連パッケージの設定
;; ggtags カスタム関数群は loads/site-elisp/my-gtags.el に定義

;;; Code:

;;;;; [Group] Code-editing - コード編集関連 ;;;;;
;;; Google C Style - Google の C スタイルガイドを適用
(use-package google-c-style
  :straight t
  :hook (c-mode-common . my/google-c-style-setup)
  :config
  (defun my/google-c-style-setup ()
    "Google C スタイルを適用し、プロジェクト標準の offset 4 を明示する.
19-language-modes.el の my/cc-mode-setup とのフック実行順に依存しないための再明示."
    (google-set-c-style)
    (google-make-newline-indent)
    (setq c-basic-offset 4))
  )

;;; Aggressive Indent - コード編集時の自動インデント調整
(use-package aggressive-indent
  :straight t
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  )

;;; Smart Newline - 改行時の自動インデントと位置調整
;; 参考: https://ainame.hateblo.jp/entry/2013/12/08/162032
(use-package smart-newline
  :straight t
  :hook ((c++-mode c-mode cc-mode emacs-lisp-mode lisp-mode) . smart-newline-mode)
  :bind (("C-m" . smart-newline))
  )

;;; move-text - テキスト行の移動機能
(use-package move-text
  :straight t
  :bind (("C-M-p" . move-text-up)
         ("C-M-n" . move-text-down))
  )

;;; Irony - C/C++ のコード補完とシンボル情報の提供
(use-package irony
  :straight t
  :defer t
  :after cc-mode
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :custom
  ;; Irony モードのインストール場所とオプションファイルの設定
  (irony-server-install-prefix    (my-set-history "irony"))
  (irony-server-options-directory (my-set-history "irony"))
  :config
  ;; irony-server 未インストール時に CAPF エラーを抑制する
  (defvar my/irony-capf-warned nil
    "Non-nil なら irony CAPF エラー警告は表示済み.")

  (defun my/safe-irony-completion-at-point (orig-fn &rest args)
    "irony エラー時に初回のみ警告を出し nil を返して CAPF として無害化する."
    (condition-case err
        (apply orig-fn args)
      (error
       (unless my/irony-capf-warned
         (setq my/irony-capf-warned t)
         (message "irony: completion error (%s)" (error-message-string err)))
       nil)))
  (unless (advice-member-p #'my/safe-irony-completion-at-point 'irony-completion-at-point)
    (advice-add 'irony-completion-at-point :around #'my/safe-irony-completion-at-point))
  )

;;; Yasnippet Snippets - 追加スニペット集
;; yasnippet の :config 内で宣言すると straight の登録とビルドが :defer 1 まで遅れ、
;; フレーム表示後に同期処理が走って入力がブロックされる。トップレベルへ出して起動中に済ませる。
;; ロード自体は :after で yasnippet に追随させる。yasnippet-snippets.el は冒頭で
;; yasnippet を require するため、無条件ロードにすると yasnippet の遅延が壊れる
;; (tests/my-test-packages.el の deferred-features 検査が検出する)。
(use-package yasnippet-snippets
  :straight t
  :after yasnippet
  :demand t
  )

;;; Yasnippet - コードスニペットの管理と挿入
(use-package yasnippet
  :straight t
  :defer 1
  :custom
  (yas-prompt-functions '(yas-completing-read-prompt yas-no-prompt)) ; スニペット選択は completing-read (vertico) を使用
  :init
  ;; ロードスニペットの設定
  ;; yasnippet ロード時に yasnippet-snippets-initialize (autoload の eval-after-load) が
  ;; 'yasnippet-snippets-dir を yas-snippet-dirs へ追記する。この setq を :config へ置くと
  ;; その追記より後に走って上書きし、追加スニペット集を失うため :init で設定する。
  ;; :custom を使わないのは yas-snippet-dirs の :set が yas-reload-all を呼ぶため。
  (setq yas-snippet-dirs
        (seq-filter #'file-exists-p
                    (list (my-set-custom "snippets")
                          ;; シンボリックリンク用
                          (my-set-custom "snippets/snippets")
                          ;; yasnippet-snippets パッケージから取得
                          ;; 注記: 以下の絞り込み指定は現状機能していない。yasnippet-snippets の自動初期化が
                          ;; snippets/ 全体を登録して全モードを JIT ロード対象にし、リーフディレクトリ指定は
                          ;; yasnippet の仕様上ロード対象にならないため（恒久対応は別タスク）。
                          ;; (my-set-straight "build/yasnippet-snippets/snippets") ; 必要最小限に絞る
                          (my-set-straight "build/yasnippet-snippets/snippets/c++-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/c++-ts-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/c-lang-common")
                          (my-set-straight "build/yasnippet-snippets/snippets/c-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/c-ts-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/cc-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/cmake-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/css-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/css-ts-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/dockerfile-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/emacs-lisp-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/git-commit-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/html-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/html-ts-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/lisp-interaction-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/lisp-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/makefile-automake-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/makefile-bsdmake-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/makefile-gmake-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/makefile-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/markdown-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/markdown-ts-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/org-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/prog-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/python-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/python-ts-mode")
                          (my-set-straight "build/yasnippet-snippets/snippets/text-mode")
                          )))
  :config
  (yas-global-mode 1)
  )

;;;;; [Group] Tags - タグナビゲーション ;;;;;
;;; Ggtags - GNU Global によるソースコード内のシンボル検索とナビゲーション
;; ggtags の xref バックエンドを経由せず、global コマンドを call-process で直接実行し
;; 結果を consult-xref → vertico で高速表示する
(use-package ggtags
  :straight t
  :defer t
  :hook ((c-mode   . ggtags-mode)
         (c++-mode . ggtags-mode))
  :bind (:map ggtags-mode-map
              ("C-t d"   . my/gtags-find-definition)     ; 関数の定義場所の検索 (define)
              ("C-t C-d" . my/gtags-find-definition)
              ("C-t u"   . my/gtags-find-references)     ; 関数の使用箇所の検索 (use)
              ("C-t C-u" . my/gtags-find-references)
              ("C-t v"   . my/gtags-find-symbol)         ; 変数の使用箇所の検索 (variable)
              ("C-t C-v" . my/gtags-find-symbol)
              ("C-t f"   . my/gtags-find-file)           ; ファイルの検索 (find)
              ("C-t C-f" . my/gtags-find-file)
              ("C-t p"   . xref-go-back)                 ; 前の履歴へ移動 (previous)
              ("C-t C-p" . xref-go-back)
              ("C-t n"   . xref-go-forward)              ; 次の履歴へ移動 (next)
              ("C-t C-n" . xref-go-forward))
  :custom
  (ggtags-update-on-save t)           ; ファイル保存時にタグを更新
  (ggtags-highlight-tag nil)          ; パフォーマンス: カーソル位置のタグハイライト無効化
  :config
  (require 'my-gtags)
  )

;; ;;; Copilot - Github copilot による補完
;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :hook (prog-mode . copilot-mode)
;;   :custom
;;   (copilot-node-executable "~/.nvm/versions/node/v21.6.1/bin/node")
;;   (warning-suppress-log-types '((copilot copilot-no-mode-indent)))
;;   (warning-suppress-types '((copilot copilot-no-mode-indent)))
;;   :bind (:map copilot-mode-map
;;               ("C-M-<return>" . copilot-complete)
;;               ("C-c c"        . copilot-clear-overlay)
;;               ("C-c C-c"      . copilot-clear-overlay)
;;               ("C-c i"        . copilot-panel-complete)
;;               ("C-c C-i"      . copilot-panel-complete)
;;               ("C-c p"        . copilot-previous-completion)
;;               ("C-c C-p"      . copilot-previous-completion)
;;               ("C-c n"        . copilot-next-completion)
;;               ("C-c C-n"      . copilot-next-completion)
;;               ("C-<return>"   . copilot-accept-completion))
;;   )

;;;;; [Group] Multi-editing - 複数編集 ;;;;;
;;; Multiple Cursors - 複数カーソルによる編集機能
(use-package multiple-cursors
  :straight t
  ;; 遅延ロードする。本体を起動時にロードすると初回描画までに約 170ms を要し、
  ;; これは外部パッケージが起動経路で消費する時間の 82% を占めていた
  ;; （docs/eval/7-elpaca-ceiling/ の実測）。
  ;; mc/* は autoload されるため、実際にコマンドを使うまでロードされない。
  :defer t
  :custom
  (mc/list-file (my-set-history "mc-lists.el"))
  ;; repeat-map と C-q プレフィックスは本体をロードせずに使える必要があるため
  ;; :config ではなく :init で定義する（束縛はシンボル参照のみで本体を要求しない）。
  :init
  ;; `repeat-mode` 用の `repeat-map` を作成
  (defvar-keymap my/mc-repeat-map
    :doc "Keymap for repeating multiple-cursors commands"
    :repeat t
    "p"  'mc/mark-previous-like-this
    "C-p" 'mc/mark-previous-like-this
    "n"  'mc/mark-next-like-this
    "C-n" 'mc/mark-next-like-this
    "*"  'mc/mark-all-like-this
    "a"  'mc/mark-all-like-this
    "C-a" 'mc/mark-all-like-this
    "d"  'mc/mark-all-like-this-dwim
    "C-d" 'mc/mark-all-like-this-dwim
    "m"  'mc/mark-more-like-this-extended
    "C-m" 'mc/mark-more-like-this-extended
    "u"  'mc/unmark-next-like-this
    "C-u" 'mc/unmark-next-like-this
    "U"  'mc/unmark-previous-like-this
    "s"  'mc/skip-to-next-like-this
    "C-s" 'mc/skip-to-next-like-this
    "S"  'mc/skip-to-previous-like-this
    "i"  'mc/insert-numbers
    "C-i" 'mc/insert-numbers
    "l"  'mc/insert-letters
    "C-l" 'mc/insert-letters
    "o"  'mc/sort-regions
    "C-o" 'mc/sort-regions
    "O"  'mc/reverse-regions)
  (global-set-key (kbd "C-q") my/mc-repeat-map) ; `C-q` をプレフィックスキーとして設定
  )

;;; Expand Region - 選択範囲をインクリメンタルに拡大・縮小
(use-package expand-region
  :straight t
  :bind (("C-," . er/expand-region))
  )

;;; symbol-overlay - シンボルのハイライトと置換
;; ※ Auto Highlight Symbol の ahs-edit-mode が Emacs 29 で正常に動作しないため置き換え
(use-package symbol-overlay
  :straight t
  :hook (prog-mode . symbol-overlay-mode)
  :bind (([f3]      . symbol-overlay-put)                   ; シンボルの永続ハイライトをトグル
         ([f4]      . symbol-overlay-remove-all)            ; ハイライトを全解除
         ("C-x C-a" . my-symbol-overlay-rename-visible)     ; ウィンドウ内のシンボルを置換
         ("C-x a"   . my-symbol-overlay-rename-in-function) ; 関数・メソッド内の置換
         ("C-x C-g" . symbol-overlay-rename))               ; バッファ全体のシンボルを置換
  :config
  ;; 現在のウィンドウ内のシンボルをリネーム
  (defun my-symbol-overlay-rename-visible ()
    "現在のウィンドウ内に表示されているシンボルのみをリネームする."
    (interactive)
    (let* ((symbol (symbol-overlay-get-symbol))
           (new-name (read-string (format "Rename '%s' to: " symbol)))
           (start (window-start))
           (end (window-end)))
      (save-excursion
        (goto-char start)
        (while (re-search-forward (regexp-quote symbol) end t)
          (replace-match new-name)))))

  ;; 現在の関数・メソッド内のシンボルをリネーム
  (defun my-symbol-overlay-rename-in-function ()
    "現在の関数やメソッド内に表示されているシンボルのみをリネームする."
    (interactive)
    (let* ((symbol (symbol-overlay-get-symbol))
           (new-name (read-string (format "Rename '%s' to: " symbol)))
           (start (cond
                   ((derived-mode-p 'c-mode 'c++-mode 'objc-mode)
                    (save-excursion (c-beginning-of-defun) (point)))
                   ((derived-mode-p 'python-mode)
                    (save-excursion (python-nav-beginning-of-defun) (point)))
                   (t (point-min)))) ;; その他のモードではファイル全体
           (end (cond
                 ((derived-mode-p 'c-mode 'c++-mode 'objc-mode)
                  (save-excursion (c-end-of-defun) (point)))
                 ((derived-mode-p 'python-mode)
                  (save-excursion (python-nav-end-of-defun) (point)))
                 (t (point-max))))) ;; その他のモードではファイル全体
      (save-excursion
        (goto-char start)
        (while (re-search-forward (regexp-quote symbol) end t)
          (replace-match new-name)))))
  )

(provide '31-editing)
;;; 31-editing.el ends here
