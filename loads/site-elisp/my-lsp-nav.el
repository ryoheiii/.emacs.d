;;; my-lsp-nav.el --- LSP 階層ナビゲーション -*- lexical-binding: t; -*-
;;; Commentary:
;; clangd (eglot) の呼び出し階層・型階層・ヘッダ/ソース切替・横断シンボル検索を
;; C-t 配下へ足す。結果は my-gtags.el と同じ Consult の選択 UI で表示し、
;; 非 LSP 環境では GNU Global / ff-find-other-file へ倒す。
;; Emacs 31 の eglot 1.24 には eglot-show-call-hierarchy / eglot-show-type-hierarchy が
;; あるが専用バッファのツリー表示になる。C-t の操作感 (Consult の選択 UI、xref 履歴) と
;; 非 LSP 環境へのフォールバックを揃えるため、clangd へ直接問い合わせる。
;; eglot は LSP 管理下のバッファでしか呼ばないので、ここでは require しない。

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'xref)
(require 'my-gtags)

;; 遅延ロード先をコンパイラへ伝える (make lint は警告をエラー扱い)。
(declare-function eglot-server-capable "eglot" (&rest feats))
(declare-function eglot--current-server-or-lose "eglot")
(declare-function eglot--signal-textDocument/didChange "eglot")
(declare-function jsonrpc-request "jsonrpc" (connection method params &rest args))
(declare-function jsonrpc-notify "jsonrpc" (connection method params))
(declare-function eglot--TextDocumentIdentifier "eglot")
(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function eglot--xref-make-match "eglot" (name uri range))
(declare-function eglot-uri-to-path "eglot" (uri))
(declare-function ggtags-find-project "ggtags")
(defvar eglot--temp-location-buffers)
(defvar jsonrpc-default-request-timeout)
(defvar eglot-advertise-cancellation)
;; let で動的束縛するには special 宣言が要る (無いと lexical 束縛になり ff に効かない)
(defvar ff-always-try-to-create)

;;; LSP 要求

(defun my/lsp-nav--protect (what thunk)
  "THUNK を実行し、`jsonrpc-error' を WHAT 付きの `user-error' にする."
  (condition-case err
      (funcall thunk)
    (jsonrpc-error
     ;; データは (説明 (jsonrpc-error-code . 数値) (jsonrpc-error-message . 文字列))。
     ;; タイムアウトには code が無い。alist-get (assq) は cons でない要素を読み飛ばす
     (let ((code (alist-get 'jsonrpc-error-code (cdr err)))
           (msg (alist-get 'jsonrpc-error-message (cdr err))))
       (user-error "LSP %s: %s" what
                   (cond ((eql code -32601)
                          "サーバーが未対応 (clangd 20 以降が必要)")
                         ((equal msg "Timed out") "応答がタイムアウト")
                         (t (or msg "不明なエラー"))))))))

(defun my/lsp-nav--raw-request (method params)
  "現在バッファの eglot サーバーへ METHOD を PARAMS で同期要求する (エラー変換なし).
`eglot--request' と同じく要求前に didChange を送り、未保存の編集も反映させる。
`eglot--request' 自体は使わない。`:timeout' を省略しても nil のまま `jsonrpc-request'
へ転送され、既定の 10 秒が無効化されて応答しないサーバーを待ち続けるうえ、
Emacs 31 ではタイムアウト (code 無し) を受けた側で型エラーになる。
ここでは `jsonrpc-default-request-timeout' を明示して有限にする。
C-g で中断したときは、Emacs 31 の `eglot--request' と同じく
`eglot-advertise-cancellation' が非 nil なら `$/cancelRequest' を送り、
clangd に計算を打ち切らせる (`:cancel-on-quit' はタイムアウトでは呼ばれない)。"
  (eglot--signal-textDocument/didChange)
  (let ((server (eglot--current-server-or-lose)))
    (jsonrpc-request server method params
                     :timeout jsonrpc-default-request-timeout
                     :cancel-on-quit
                     (and eglot-advertise-cancellation
                          (lambda (id)
                            (jsonrpc-notify server '$/cancelRequest `(:id ,id)))))))

(defun my/lsp-nav--request (method params)
  "現在バッファの eglot サーバーへ METHOD を PARAMS で同期要求する.
`jsonrpc-error' は `user-error' へ変換する。"
  (my/lsp-nav--protect
   (substring (symbol-name method) 1)
   (lambda () (my/lsp-nav--raw-request method params))))

(defun my/lsp-nav--prepare (what capability method)
  "カーソル位置で prepare METHOD を実行し、階層アイテムのリストを返す.
WHAT は利用者向けの機能名、CAPABILITY はサーバーに要る capability。"
  (unless (my/gtags--lsp-p)
    (user-error "%s は LSP (clangd) 管理下のバッファでのみ使えます" what))
  (unless (my/lsp-nav--protect what (lambda () (eglot-server-capable capability)))
    (user-error "LSP サーバーが %s に対応していません" what))
  (or (append (my/lsp-nav--request method (eglot--TextDocumentPositionParams)) nil)
      (user-error "カーソル位置に %s の対象がありません" what)))

;;; xref 変換

(defun my/lsp-nav--xref (label item range)
  "階層アイテム ITEM の RANGE を指す xref を作り、要約に LABEL を前置する.
行テキストの取得と UTF-16 桁の変換は `eglot--xref-make-match' に任せる。"
  (let ((xref (eglot--xref-make-match (plist-get item :name)
                                      (plist-get item :uri) range)))
    (xref-make-match (concat (propertize label 'face 'font-lock-function-name-face)
                             " " (xref-item-summary xref))
                     (xref-item-location xref)
                     (xref-match-length xref))))

(defun my/lsp-nav--collect (fn)
  "FN が `eglot--xref-make-match' で作った xref を返し、eglot の一時バッファを片付ける.
`eglot--collecting-xrefs' はマクロなので使わず、同じ後始末を手で行う。"
  (unwind-protect
      (funcall fn)
    (maphash (lambda (_uri buffer) (kill-buffer buffer)) eglot--temp-location-buffers)
    (clrhash eglot--temp-location-buffers)))

(defun my/lsp-nav--show (what xrefs)
  "XREFS を履歴に積みつつ Consult で表示する (`my/gtags--find-via-global' と同じ経路).
空なら WHAT が見つからない旨を表示する。"
  (if xrefs
      (let ((xref-show-xrefs-function #'my/gtags--show))
        (xref-show-xrefs (lambda () xrefs) nil))
    (message "%s は見つかりません" what)))

;;; 呼び出し階層

(defun my/lsp-find-callers (&optional prefix)
  "カーソル位置の関数の呼び出し元を検索する.
LSP 管理下なら clangd の callHierarchy/incomingCalls を使い、呼び出し箇所ごとに
1 件ずつ表示する。非 LSP 環境と PREFIX (C-u) 付きは `my/gtags-find-references'
へ委譲する (C-u は GNU Global 強制。`C-u C-t u' と同じ)。"
  (interactive "P")
  (if (or prefix (not (my/gtags--lsp-p)))
      (my/gtags-find-references prefix)
    (let* ((item (car (my/lsp-nav--prepare "呼び出し元" :callHierarchyProvider
                                           :textDocument/prepareCallHierarchy)))
           (calls (my/lsp-nav--request :callHierarchy/incomingCalls (list :item item))))
      (my/lsp-nav--show
       "呼び出し元"
       (my/lsp-nav--collect
        (lambda ()
          (let (xrefs)
            (seq-doseq (call calls)
              (let ((from (plist-get call :from)))
                (seq-doseq (range (plist-get call :fromRanges))
                  (push (my/lsp-nav--xref (concat (plist-get from :name) ":") from range)
                        xrefs))))
            (nreverse xrefs))))))))

(defun my/lsp-find-callees ()
  "カーソル位置の関数の呼び出し先を検索する (LSP のみ).
clangd は 20 以降で callHierarchy/outgoingCalls に対応する。それより前の版では
サーバー未対応のエラーになる。"
  (interactive)
  (let* ((item (car (my/lsp-nav--prepare "呼び出し先" :callHierarchyProvider
                                         :textDocument/prepareCallHierarchy)))
         (calls (my/lsp-nav--request :callHierarchy/outgoingCalls (list :item item))))
    (my/lsp-nav--show
     "呼び出し先"
     (my/lsp-nav--collect
      (lambda ()
        (mapcar (lambda (call)
                  (let ((to (plist-get call :to)))
                    (my/lsp-nav--xref (concat (plist-get to :name) ":")
                                      to (plist-get to :selectionRange))))
                calls))))))

;;; 型階層

(defun my/lsp-find-type-hierarchy ()
  "カーソル位置の型の直接の基底と派生を一覧する (LSP のみ).
要約の先頭に [基底] / [派生] を付ける。多段は移動先で再実行する。"
  (interactive)
  (let ((item (car (my/lsp-nav--prepare "型階層" :typeHierarchyProvider
                                        :textDocument/prepareTypeHierarchy))))
    (my/lsp-nav--show
     "基底・派生"
     (my/lsp-nav--collect
      (lambda ()
        (cl-loop for (label . method) in '(("[基底]" . :typeHierarchy/supertypes)
                                           ("[派生]" . :typeHierarchy/subtypes))
                 append (mapcar (lambda (type)
                                  (my/lsp-nav--xref label type
                                                    (plist-get type :selectionRange)))
                                (my/lsp-nav--request method (list :item item)))))))))

;;; ヘッダ/ソース切替

(defun my/lsp-nav--switch-source-header ()
  "clangd 拡張 switchSourceHeader で対になるファイルのパスを返す.
対が無い (null) か要求に失敗したときは nil を返し、呼び出し側が ff へ倒す。"
  (let ((uri (condition-case err
                 (my/lsp-nav--raw-request :textDocument/switchSourceHeader
                                          (eglot--TextDocumentIdentifier))
               (jsonrpc-error
                (message "switchSourceHeader を利用できません (%s)。ff-find-other-file を使います"
                         (or (alist-get 'jsonrpc-error-message (cdr err)) "不明なエラー"))
                nil))))
    (and (stringp uri) (eglot-uri-to-path uri))))

(defun my/lsp-find-other-file (&optional prefix)
  "ヘッダとソースを切り替える.
LSP 管理下なら clangd の switchSourceHeader を使う (src/ と include/ のように
別ディレクトリにある対も見つかる)。対が無いとき、非 LSP 環境、PREFIX (C-u) 付きは
`ff-find-other-file' を使う (見つからなくても新規ファイルは作らない)。
移動前の位置は xref の履歴に積むので `C-t p' で戻れる。"
  (interactive "P")
  (let ((target (and (not prefix)
                     (my/gtags--lsp-p)
                     (my/lsp-nav--switch-source-header))))
    (xref-push-marker-stack)
    (if target
        (find-file target)
      (let ((ff-always-try-to-create nil))
        (ff-find-other-file)))))

;;; 横断シンボル検索

(defun my/lsp-nav--workspace-symbols (pattern)
  "workspace/symbol で PATTERN に一致するシンボルの xref を返す (LSP のみ).
eglot の xref バックエンド (`xref-backend-apropos') は内部の要求ラッパーを使い
タイムアウトが無期限になるため、同じ要求を `my/lsp-nav--request' で送る。
応答は SymbolInformation の配列で、range を持たない要素は読み飛ばす。"
  (unless (my/lsp-nav--protect "workspace/symbol"
                               (lambda () (eglot-server-capable :workspaceSymbolProvider)))
    (user-error "LSP サーバーが workspace/symbol に対応していません"))
  (my/lsp-nav--collect
   (lambda ()
     (delq nil
           (mapcar (lambda (symbol)
                     (let* ((location (plist-get symbol :location))
                            (range (plist-get location :range)))
                       (and range
                            (eglot--xref-make-match (plist-get symbol :name)
                                                    (plist-get location :uri) range))))
                   (my/lsp-nav--request :workspace/symbol (list :query pattern)))))))

(defun my/lsp-find-workspace-symbol (pattern)
  "PATTERN でプロジェクト横断のシンボルを検索する.
LSP 管理下は workspace/symbol (名前のファジー一致)、それ以外で GTAGS があれば
GNU Global の grep 検索 (正規表現) を使う。`xref-find-apropos' を経由しないのは、
ggtags バックエンドがカーソル位置の語が GTAGS に無いと選ばれず、Global へ
届かないため。ggtags の引用処理を避け、検索式を Global へそのまま渡す。"
  (interactive
   (list (read-string "Find symbol (pattern): " (thing-at-point 'symbol t)
                      'my/gtags-history)))
  (when (string-blank-p pattern)
    (user-error "検索する名前を入力してください"))
  (let ((xrefs (cond ((my/gtags--lsp-p)
                      (my/lsp-nav--workspace-symbols pattern))
                     ((and (fboundp 'ggtags-find-project) (ggtags-find-project))
                      (my/gtags--run "--grep" pattern))
                     (t (user-error "シンボル検索には eglot (clangd) か GTAGS が必要です")))))
    (my/lsp-nav--show (format "シンボル %s" pattern) xrefs)))

(provide 'my-lsp-nav)
;;; my-lsp-nav.el ends here
