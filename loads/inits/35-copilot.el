;;; 35-copilot.el --- GitHub Copilot の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; GitHub Copilot のインライン補完と Chat 機能の設定
;; 無効化: custom.el に (setq my/copilot-enabled nil) を追加
;; Chat モデル変更: C-c J m (copilot-chat-set-model) で切り替え

;;; Code:

;;;;; [Group] Copilot Config - 設定グループとトグル ;;;;;
(defgroup my/copilot nil
  "GitHub Copilot の設定."
  :prefix "my/copilot-"
  :group 'tools)

(defcustom my/copilot-enabled t
  "Non-nil なら GitHub Copilot を有効化する.
アカウントがない環境では custom.el で nil に設定する."
  :type 'boolean
  :group 'my/copilot)

;;;;; [Group] Node.js Path - パス検出 ;;;;;
(defun my/copilot--prepend-to-path (dir)
  "DIR を `exec-path' と環境変数 PATH の先頭に移動する.
既に存在する場合は一度除去してから先頭に再挿入し、優先順を保証する."
  (when (and dir (file-directory-p dir))
    ;; 既存エントリを除去してから先頭に追加
    (setq exec-path (delete dir exec-path))
    (push dir exec-path)
    (let* ((current (or (getenv "PATH") ""))
           (entries (split-string current ":" t))
           (cleaned (delete dir entries)))
      (setenv "PATH" (string-join (cons dir cleaned) ":")))))

(defun my/copilot--resolve-nvm-alias (alias-value nvm-dir &optional depth)
  "ALIAS-VALUE を再帰的に解決し、バージョン文字列を返す.
NVM-DIR は nvm のルート。DEPTH はループ防止用（最大 10）.
数値バージョン（例: \"22\", \"v22.14.0\"）はそのまま返す。
名前エイリアス（例: \"lts/*\", \"node\"）は ~/.nvm/alias/ から再帰解決する."
  (let ((depth (or depth 0)))
    (when (< depth 10)
      (cond
       ;; 数値バージョン → そのまま返す
       ((string-match-p "\\`v?[0-9]+\\(?:\\.[0-9]+\\)*\\'" alias-value)
        alias-value)
       ;; 名前エイリアス → ~/.nvm/alias/<name> を読んで再帰
       ((not (string-empty-p alias-value))
        (let ((alias-file (expand-file-name (concat "alias/" alias-value) nvm-dir)))
          (when (file-exists-p alias-file)
            (let ((next (string-trim
                         (with-temp-buffer
                           (insert-file-contents alias-file)
                           (buffer-string)))))
              (unless (string-empty-p next)
                (my/copilot--resolve-nvm-alias next nvm-dir (1+ depth)))))))))))

(defun my/copilot-setup-node-path ()
  "fnm/nvm の Node.js パスを検出し PATH 先頭に追加する.
優先順: fnm > nvm。低優先度から処理し、最後に追加したものが先頭に来る."
  ;; 1. nvm（フォールバック: default エイリアスから解決）
  (let* ((nvm-dir (or (getenv "NVM_DIR") (expand-file-name ".nvm" "~")))
         (nvm-alias-file (expand-file-name "alias/default" nvm-dir)))
    (when (file-exists-p nvm-alias-file)
      (let* ((alias-value (string-trim
                           (with-temp-buffer
                             (insert-file-contents nvm-alias-file)
                             (buffer-string))))
             ;; 再帰的にエイリアスを解決（"lts/*" → "lts/krypton" → "v24.13.0" 等）
             (resolved (my/copilot--resolve-nvm-alias alias-value nvm-dir)))
        (when resolved
          (let* ((version (if (string-prefix-p "v" resolved)
                              resolved
                            (concat "v" resolved)))
                 ;; 部分バージョン（"v22"）→ ディレクトリ glob で最新を取得
                 (node-dirs (file-expand-wildcards
                             (expand-file-name
                              (concat "versions/node/" version "*/bin") nvm-dir)))
                 ;; セマンティックバージョン比較（string< だと v22.9 > v22.14 になる）
                 ;; version< は "v" 付きだと Invalid version syntax になるため除去
                 (node-bin (car (last (sort node-dirs
                                            (lambda (a b)
                                              (let ((va (replace-regexp-in-string "\\`v" ""
                                                         (file-name-nondirectory
                                                          (directory-file-name (file-name-directory a)))))
                                                    (vb (replace-regexp-in-string "\\`v" ""
                                                         (file-name-nondirectory
                                                          (directory-file-name (file-name-directory b))))))
                                                (version< va vb))))))))
            (when node-bin
              (my/copilot--prepend-to-path node-bin)))))))
  ;; 2. fnm（最優先: 最後に prepend → PATH の先頭に来る）
  (my/copilot--prepend-to-path
   (expand-file-name ".local/share/fnm/aliases/default/bin" "~")))

;; use-package 評価前に実行（:if の executable-find に間に合わせる）
(when my/copilot-enabled
  (my/copilot-setup-node-path))

;;;;; [Group] Copilot - インライン補完 ;;;;;
;;; Copilot - GitHub Copilot によるインライン補完
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :if (and my/copilot-enabled (executable-find "node"))
  :hook (prog-mode . copilot-mode)
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
      "C-c j m" "toggle-mode"))
  )

;;;;; [Group] Copilot Chat - チャット ;;;;;
;;; request — copilot-chat の依存パッケージ
(use-package request
  :straight t
  :if my/copilot-enabled
  :defer t
  :custom
  (request-storage-directory (my-set-history "request/")))

;;; Copilot Chat - GitHub Copilot Chat によるコード分析・対話
(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :if (and my/copilot-enabled (executable-find "node"))
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
      "C-c J q" "reset"))

  ;; org-mode は tab-width 8 を要求する（copilot-chat の org フロントエンド等で警告抑止）
  (add-hook 'org-mode-hook (lambda () (setq tab-width 8)))
  )

(provide '35-copilot)
;;; 35-copilot.el ends here
