;;; 35-copilot.el --- GitHub Copilot の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; GitHub Copilot のインライン補完と Chat 機能の設定
;;
;; 有効・無効の切り替えは `my/copilot-enabled' で行う (既定は 'auto)。
;;   M-x my/copilot-toggle                  … 切り替えて custom.el へ保存
;;   M-x customize-variable my/copilot-enabled … 3 値から選ぶ
;; 環境が揃わない場合は段階的に縮退する (C/C++ の三段フォールバックと同じ方針)。
;;   auto で Node.js が無い  → 宣言をロードしない（クローンも行わない）
;;   language server が無い  → copilot-mode を有効化しない (M-x copilot-install-server)
;; パッケージが有効な環境では C-c j m (copilot-mode) で現在のバッファだけ切り替える。
;; Chat モデル変更: C-c J m (copilot-chat-set-model) で切り替え

;;; Code:

;; 遅延ロード先とプラットフォーム固有定義をコンパイラへ伝える。
(declare-function my/copilot--extend-jsonrpc-timeout "35-copilot")

;;; 設定グループとトグル
(defgroup my/copilot nil
  "GitHub Copilot の設定."
  :prefix "my/copilot-"
  :group 'tools)

(defcustom my/copilot-enabled 'auto
  "GitHub Copilot を有効にするか.
auto … Node.js が見つかる環境でだけ有効にする (既定)
t    … Node.js の有無に関わらず有効にする
nil  … 完全に無効。パッケージのクローンとビルドも行わない

Copilot を使えない環境 (Node.js を導入できない、GitHub へ到達できない等) では
nil にする。`my/copilot-toggle' から切り替えると custom.el へ保存される."
  :type '(choice (const :tag "自動判定 (Node.js があれば有効)" auto)
                 (const :tag "常に有効" t)
                 (const :tag "無効" nil))
  :group 'my/copilot)

(defconst my/copilot-install-dir (my-set-package "copilot/")
  "copilot language server の導入先。`copilot-install-dir' と一致させる。
既定の .emacs.d/.cache/ はリポジトリ直下に自動生成物を作るため使わない。")

;;; パス検出
;; Node.js の検出処理は my-node-path.el にまとめる。実行は可用性判定の直前。
(require 'my-node-path)

;;; 可用性判定と切り替え
;; PATH の整備は可用性判定より前に行う
;; (nvm/fnm 配下の node を executable-find が拾えるようにするため)。
(when my/copilot-enabled
  (my/copilot-setup-node-path))

(defun my/copilot-available-p ()
  "この環境で Copilot のパッケージ宣言をロードしてよいなら non-nil.
`my/copilot-enabled' が \\='auto のときは Node.js の有無で決める."
  (pcase my/copilot-enabled
    ('nil nil)
    ('auto (and (executable-find "node") t))
    (_ t)))

(defun my/copilot-toggle ()
  "Copilot の有効・無効を切り替え、`custom-file' へ保存する.
有効化は \\='auto (Node.js があるときだけ動く) へ倒す。
パッケージのロード可否は起動時に決まるため、反映には再起動が必要。
一時的に補完だけ止めたい場合は C-c j m (`copilot-mode') を使う."
  (interactive)
  (let ((next (if my/copilot-enabled nil 'auto)))
    (customize-save-variable 'my/copilot-enabled next)
    (message "Copilot: %s。反映には Emacs の再起動が必要です"
             (if next "有効 (auto)" "無効"))))

(defun my/copilot-server-available-p ()
  "copilot language server の実体が見つかるなら non-nil.
copilot 本体をロードせずに判定するため、`copilot-server-executable' と
同じ探索経路 (導入先直下と bin/、および PATH 上) を自前でたどる."
  (let ((exec-path (append (list my/copilot-install-dir
                                 (expand-file-name "bin" my/copilot-install-dir))
                           exec-path)))
    (and (executable-find "copilot-language-server") t)))

(when (my/copilot-available-p)

  ;; copilot / copilot-chat の依存解決は、組み込みで足りるパッケージでも
  ;; レシピリポジトリに存在すればそちらを優先してクローンする
  ;; (straight--convert-recipe が built-in へ落ちるのはレシピが見つからないときだけ)。
  ;; その結果 straight の build が組み込み版を load-path 上で覆い隠し、
  ;; 24-org.el の `:straight nil' や eglot が使う jsonrpc の前提が崩れる。
  ;; Emacs 31.1 の組み込み版が要件を満たすものだけ built-in へ固定する。
  ;;   org           9.8.7  >= 9.4.6  (copilot-chat の要件)
  ;;   jsonrpc       1.0.29 >= 1.0.14 (copilot の要件)
  ;;   track-changes 1.5    >= 1.4    (copilot の要件。straight 版 1.5 とコードも同一)
  ;; transient (0.13.3 >= 0.8.3) も要件を満たすが、magit が使う transient の
  ;; 差し替えを含めて未検証のため対象にしない。
  ;; compat / seq は全パッケージ共通の依存のため init.el で固定している。
  (dolist (pkg '(org jsonrpc track-changes))
    (straight-override-recipe (list pkg :type 'built-in)))

;;; インライン補完
;;;; Copilot - GitHub Copilot によるインライン補完
  ;; copilot.el リポジトリは独自の copilot-chat.el を同梱しており、そのまま
  ;; ビルドすると chep/copilot-chat.el と同名 feature (copilot-chat) が二重になる。
  ;; どちらがロードされるかは load-path 順に依存し、非互換な実装が選ばれ得るため
  ;; 同梱版を recipe から除外する（Chat は chep 版に一本化する）。
  (use-package copilot
    :straight (:host github :repo "copilot-emacs/copilot.el"
                     :files ("*.el" (:exclude "copilot-chat.el")))
    :hook (prog-mode . my/copilot-maybe-enable)
    :bind (;; copilot-mode OFF 時もトグルできるようグローバルに配置
           ("C-c j m" . copilot-mode)                             ; モード ON/OFF
           :map copilot-mode-map
           ;; 全キーを copilot-mode-map に統一（which-key 常時表示のため）
           ("C-c j j" . copilot-accept-completion)                ; 補完を確定
           ("C-c j w" . copilot-accept-completion-by-word)        ; 単語単位で確定
           ("C-c j l" . copilot-accept-completion-by-line)        ; 行単位で確定
           ("C-c j n" . copilot-next-completion)                  ; 次の候補
           ("C-c j p" . copilot-previous-completion)              ; 前の候補
           ("C-c j c" . copilot-complete)                         ; 手動で補完トリガー
           ("C-c j s" . copilot-panel-complete)                   ; パネルで複数候補
           ("C-c j d" . copilot-diagnose)                         ; 診断情報
           ("C-c j e" . copilot-select-completion-model))         ; モデル選択
    :custom
    (copilot-idle-delay 0)                                        ; 即時補完
    (copilot-indent-offset-warning-disable t)
    (copilot-install-dir my/copilot-install-dir)                   ; language server の保存先
    :init
    (defun my/copilot-maybe-enable ()
      "language server が導入済みの環境でだけ `copilot-mode' を有効にする.
未導入のまま有効化すると `copilot--start-server' が user-error を送出し、
prog-mode のバッファを開くたびにエラーになる。導入は M-x copilot-install-server。"
      (when (my/copilot-server-available-p)
        (copilot-mode 1)))
    :config
    ;; WSL2 環境では Language Server の起動が遅いためタイムアウトを延長
    ;; copilot--request はマクロのため advice 不可 → jsonrpc-request を advice し
    ;; copilot 接続時のみ timeout を上書きする（eglot 等への影響を回避）
    (defun my/copilot--extend-jsonrpc-timeout (orig-fn connection &rest args)
      "Copilot 接続時のみ jsonrpc リクエストタイムアウトを 30 秒に延長する."
      (if (and (boundp 'copilot--connection)
               (eq connection copilot--connection))
          (let ((jsonrpc-default-request-timeout 30))
            (apply orig-fn connection args))
        (apply orig-fn connection args)))
    (unless (advice-member-p #'my/copilot--extend-jsonrpc-timeout 'jsonrpc-request)
      (advice-add 'jsonrpc-request :around #'my/copilot--extend-jsonrpc-timeout))

    ;; warning-suppress を上書きせず追加（00-core.el の既存設定を保護）
    (add-to-list 'warning-suppress-log-types '(copilot copilot-no-mode-indent))
    (add-to-list 'warning-suppress-types '(copilot copilot-no-mode-indent))

    ;; which-key 用のプレフィックス説明（C-c j を押すだけでサブキー一覧が表示される）
    (with-eval-after-load 'which-key
      (which-key-add-keymap-based-replacements copilot-mode-map
        "C-c j"   "Copilot"
        "C-c j j" "accept"
        "C-c j w" "accept-word"
        "C-c j l" "accept-line"
        "C-c j n" "next"
        "C-c j p" "previous"
        "C-c j c" "complete"
        "C-c j s" "panel"
        "C-c j d" "diagnose"
        "C-c j e" "select-model")
      (which-key-add-key-based-replacements
        "C-c j"   "Copilot"
        "C-c j m" "toggle-mode")))

;;; チャット
;;;; request - copilot-chat の依存パッケージ
  (use-package request
    :straight t
    :defer t
    :custom
    (request-storage-directory (my-set-history "request/")))

;;;; Copilot Chat - GitHub Copilot Chat によるコード分析・対話
  (use-package copilot-chat
    :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
    :commands (copilot-chat-display copilot-chat-transient
               copilot-chat-explain copilot-chat-review
               copilot-chat-fix copilot-chat-optimize
               copilot-chat-test copilot-chat-doc
               copilot-chat-reset copilot-chat-insert-commit-message
               copilot-chat-add-current-buffer copilot-chat-add-workspace
               copilot-chat-list copilot-chat-ask-and-insert
               copilot-chat-explain-defun copilot-chat-review-whole-buffer
               copilot-chat-custom-prompt-selection copilot-chat-custom-prompt-mini-buffer
               copilot-chat-set-model copilot-chat-cancel)
    :bind (;; C-c J プレフィックス（Copilot Chat 操作）
           ;; --- チャット表示 ---
           ("C-c J J" . copilot-chat-display)                   ; チャットを開く
           ("C-c J h" . copilot-chat-hide)                      ; チャットを隠す
           ("C-c J t" . copilot-chat-transient)                  ; トランジェントメニュー
           ;; --- コード分析（リージョン選択） ---
           ("C-c J e" . copilot-chat-explain)                    ; コード説明
           ("C-c J r" . copilot-chat-review)                     ; コードレビュー
           ("C-c J f" . copilot-chat-fix)                        ; バグ修正提案
           ("C-c J o" . copilot-chat-optimize)                   ; 最適化提案
           ("C-c J T" . copilot-chat-test)                       ; テスト生成
           ("C-c J d" . copilot-chat-doc)                        ; ドキュメント生成
           ;; --- コード分析（選択不要） ---
           ("C-c J E" . copilot-chat-explain-defun)              ; カーソル位置の関数を説明
           ("C-c J R" . copilot-chat-review-whole-buffer)        ; バッファ全体をレビュー
           ;; --- プロンプト・挿入 ---
           ("C-c J p" . copilot-chat-custom-prompt-selection)    ; カスタムプロンプト（リージョン付）
           ("C-c J P" . copilot-chat-custom-prompt-mini-buffer)  ; ミニバッファから質問
           ("C-c J i" . copilot-chat-ask-and-insert)             ; 回答をカーソル位置に挿入
           ("C-c J s" . copilot-chat-send-to-buffer)            ; コードブロックをバッファに送信
           ("C-c J w" . copilot-chat-copy-code-at-point)        ; コードブロックをコピー
           ;; --- コンテキスト管理 ---
           ("C-c J a" . copilot-chat-add-current-buffer)         ; バッファをコンテキストに追加
           ("C-c J A" . copilot-chat-add-workspace)              ; ワークスペース全体を追加
           ("C-c J x" . copilot-chat-del-current-buffer)         ; バッファをコンテキストから除去
           ("C-c J l" . copilot-chat-list)                       ; コンテキスト一覧
           ;; --- その他 ---
           ("C-c J g" . copilot-chat-insert-commit-message)      ; コミットメッセージ生成
           ("C-c J m" . copilot-chat-set-model)                  ; モデル切り替え
           ("C-c J c" . copilot-chat-cancel)                     ; 応答を中断
           ("C-c J q" . copilot-chat-reset))                     ; チャットリセット
    :custom
    ;; モデルはサーバー側デフォルトに委ねる（C-c J m で切り替え可能）
    (copilot-chat-frontend 'org)                                 ; org-mode フロントエンド
    ;; org フロントエンド用システムプロンプト（日本語回答指示を末尾に追加）
    (copilot-chat-org-prompt
     "The user works in an IDE called Emacs which has an org major mode for keeping notes, authoring documents, computational notebooks, literate programming, maintaining to-do lists, planning projects, and more — in a fast and effective plain text system.

Use only Emacs org-mode formatting in your answers.
When using heading to structure your answer, please start at level 3 (i.e with 3 stars or more)
Make sure to include the programming language name at the start of the org-mode code blocks.
This is an example of python code block in emacs org-mode syntax:
#+BEGIN_SRC python
def hello_world():
	print('Hello, World!')
#+END_SRC
Avoid wrapping the whole response in the block code.

Don't forget the most important rule when you are formatting your response: use emacs org-mode syntax only.

Always respond in Japanese.")
    :config
    ;; which-key 用のプレフィックス説明
    (with-eval-after-load 'which-key
      (which-key-add-key-based-replacements
        "C-c J"   "Copilot Chat"
        "C-c J J" "open-chat"
        "C-c J h" "hide-chat"
        "C-c J t" "transient-menu"
        ;; コード分析（リージョン選択）
        "C-c J e" "explain"
        "C-c J r" "review"
        "C-c J f" "fix"
        "C-c J o" "optimize"
        "C-c J T" "test"
        "C-c J d" "doc"
        ;; コード分析（選択不要）
        "C-c J E" "explain-defun"
        "C-c J R" "review-buffer"
        ;; プロンプト・挿入
        "C-c J p" "custom-prompt"
        "C-c J P" "prompt-mini"
        "C-c J i" "ask+insert"
        "C-c J s" "send-to-buf"
        "C-c J w" "copy-code"
        ;; コンテキスト管理
        "C-c J a" "add-buffer"
        "C-c J A" "add-workspace"
        "C-c J x" "del-buffer"
        "C-c J l" "list-buffers"
        ;; その他
        "C-c J g" "commit-msg"
        "C-c J m" "set-model"
        "C-c J c" "cancel"
        "C-c J q" "reset"))))

(provide '35-copilot)
;;; 35-copilot.el ends here
