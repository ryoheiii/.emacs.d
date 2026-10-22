;;; my-test-deferred.el --- 遅延ロード設定の回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; :defer N で遅延される use-package の :config が正しく動くことを検証する。
;; バッチでは idle timer が発火しないため、require で :config を発火させ、
;; 対応するグローバルモードが有効になることを確認する。
;; 注意: このテストは対象 feature をロードするため、未ロードを検証する
;; my-test-packages.el (:invariant) とは別プロセス (test-deferred) で実行する。

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; 補完の起動待ち
(ert-deftest my-test-deferred-corfu-auto-settings ()
  "Corfu の自動補完設定をフックで上書きせず、補完スタイルだけをローカルにする。"
  :tags '(:deferred)
  (require 'corfu)
  (with-temp-buffer
    (corfu-mode 1)
    (should (= corfu-auto-delay 0.2))
    (should (= corfu-auto-prefix 2))
    (should-not (local-variable-p 'corfu-auto-delay))
    (should-not (local-variable-p 'corfu-auto-prefix))
    (should (local-variable-p 'completion-styles))
    (should (equal completion-styles '(orderless-fast basic)))
    (should (memq 'corfu-auto--post-command post-command-hook))))

(ert-deftest my-test-deferred-copilot-idle-delay ()
  "Copilot は入力停止から 0.2 秒待って補完する。"
  :tags '(:deferred)
  (skip-unless (my/copilot-available-p))
  (require 'copilot)
  (should (= copilot-idle-delay 0.2)))

;;; 組み込みコマンドの初回ロード
(ert-deftest my-test-deferred-ediff-settings ()
  :tags '(:deferred)
  ;; 標準のコマンド入口からロードした後もウィンドウ配置を保つ。
  (should (commandp 'ediff-buffers))
  (autoload-do-load (symbol-function 'ediff-buffers) 'ediff-buffers)
  (should (eq ediff-window-setup-function 'ediff-setup-windows-plain))
  (should (eq ediff-split-window-function 'split-window-horizontally)))

(ert-deftest my-test-deferred-ispell-settings ()
  :tags '(:deferred)
  (should (commandp 'ispell-buffer))
  (autoload-do-load (symbol-function 'ispell-buffer) 'ispell-buffer)
  (should (equal ispell-dictionary "en_US"))
  (should ispell-silently-savep)
  (should (equal ispell-skip-region-alist '(("[^\000-\377]+"))))
  (cond
   ((executable-find "hunspell")
    (should (equal ispell-program-name "hunspell"))
    (should (equal ispell-extra-args '("-d" "en_US"))))
   ((executable-find "aspell")
    (should (equal ispell-program-name "aspell"))
    (should (equal ispell-extra-args '("--sug-mode=ultra")))))
  (should (advice-member-p #'my/safe-ispell-completion-at-point
                           'ispell-completion-at-point))
  (let ((my/ispell-capf-warned nil)
        (messages 0))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) (cl-incf messages))))
      (dotimes (_ 2)
        (should-not
         (my/safe-ispell-completion-at-point
          (lambda () (error "テスト用の辞書エラー")))))
      (should (= messages 1)))))

;;; 一致なしのキャッシュと一時エラーの再試行
(ert-deftest my-test-deferred-flx-cache-results ()
  :tags '(:deferred)
  (require 'flx)
  (let ((my/flx-cache (make-hash-table :test 'equal))
        (calls 0))
    (cl-letf (((symbol-function 'flx-score)
               (lambda (_str query &rest _)
                 (cl-incf calls)
                 (when (equal query "match") '(42)))))
      (dolist (query '("missing" "match"))
        (dotimes (_ 2)
          (should (equal (my/get-flx-score "candidate" query)
                         (if (equal query "match") 42 nil)))))
      (should (= calls 2))
      (my/clear-flx-cache)
      (should-not (my/get-flx-score "candidate" "missing"))
      (should (= calls 3)))))

(ert-deftest my-test-deferred-flx-cache-retries-errors ()
  :tags '(:deferred)
  (require 'flx)
  (let ((my/flx-cache (make-hash-table :test 'equal))
        (calls 0))
    (cl-letf (((symbol-function 'flx-score)
               (lambda (&rest _)
                 (cl-incf calls)
                 (if (= calls 1) (error "テスト用の一時エラー") '(42)))))
      (should-not (my/get-flx-score "candidate" "query"))
      (should (= (my/get-flx-score "candidate" "query") 42))
      (should (= (my/get-flx-score "candidate" "query") 42))
      (should (= calls 2)))))

;;; 遅延ロード後のグローバルモード有効化
;; :if ガードが環境依存のもの (xclip, migemo) と GUI 限定のもの
;; (pulsar, spacious-padding, nyan-mode) は対象外とする。
(defconst my-test-deferred--feature-modes
  '((popwin           . popwin-mode)
    (perfect-margin   . perfect-margin-mode)
    (page-break-lines . global-page-break-lines-mode)
    (minions          . minions-mode)
    (yasnippet        . yas-global-mode)
    (diff-hl          . global-diff-hl-mode))
  ":defer 対象の feature と、ロード後に有効化されるべきグローバルモード。")

(ert-deftest my-test-deferred-config-activation ()
  :tags '(:deferred)
  (dolist (entry my-test-deferred--feature-modes)
    (require (car entry))
    (should (default-value (cdr entry)))))

(ert-deftest my-test-deferred-copilot-menu-alignment-before-magit ()
  "Magit を開かなくても全角説明の列揃え設定が届くこと。"
  :tags '(:deferred)
  (should-not (featurep 'magit))
  (require 'copilot-chat-transient)
  (should transient-align-variable-pitch)
  (should-not (featurep 'magit)))

;;; Difftastic の Magit メニュー
(ert-deftest my-test-deferred-difftastic-magit-menu-before-load ()
  :tags '(:deferred)
  (should-not (featurep 'magit))
  (should-not (featurep 'magit-diff))
  (should-not (featurep 'difftastic))
  ;; 直呼びの autoload 先だけを先に読み、その後の Magit ロードでも検査する。
  (dolist (feature '(magit-diff magit))
    (require feature)
    (should (eq (featurep 'magit) (eq feature 'magit)))
    (should-not (featurep 'difftastic))
    ;; 下位メニューの説明は magit 本体より先の初回ロードでも届く。
    (should (equal (plist-get (cdr (transient-get-suffix 'magit-diff "u"))
                              :description)
                   "未追加の差分"))
    ;; メニュー項目の登録と定義の取得だけでは、本体をロードしない。
    (dolist (entry '(("D" . difftastic-magit-diff)
                     ("S" . difftastic-magit-show)))
      (let ((suffix (transient-get-suffix 'magit-diff (car entry))))
        (should (eq (plist-get (cdr suffix) :command) (cdr entry))))
      (should (commandp (cdr entry)))
      (should (autoloadp (symbol-function (cdr entry)))))
    (should (eq (featurep 'magit) (eq feature 'magit)))
    (should-not (featurep 'difftastic)))
  (dolist (key '("D" "S"))
    (should (eq (lookup-key magit-blame-read-only-mode-map (kbd key))
                'difftastic-magit-show))))

;;; yasnippet のスニペットディレクトリ
;; 31-editing.el の :init は 'yasnippet-snippets-dir を yas-snippet-dirs へ
;; あらかじめ入れる。yasnippet-snippets-initialize (autoload の eval-after-load) は
;; (member 'yasnippet-snippets-dir yas-snippet-dirs) が真なら何もしないため、
;; この事前投入により初期化時の yas--load-snippet-dirs が抑止され、
;; 全ディレクトリ走査は :config の yas-global-mode による 1 回だけで済む。
;; 事前投入が外れると走査が 2 回に戻る (実測 206ms -> 464ms)。
(defconst my-test-deferred--yas-dirs-before-load
  (and (boundp 'yas-snippet-dirs) yas-snippet-dirs)
  "yasnippet をロードする前の `yas-snippet-dirs'。
このファイルのロード時点では、まだどのテストも yasnippet を require していない。")

(defvar my-test-deferred--non-string-value nil
  "値が文字列でない動的変数。`my/yas-resolve-snippet-dirs' の型検査に使う。")

(ert-deftest my-test-deferred-yasnippet-snippets-dir-preseeded ()
  :tags '(:deferred)
  (should (memq 'yasnippet-snippets-dir my-test-deferred--yas-dirs-before-load)))

(ert-deftest my-test-deferred-yasnippet-snippets-dir-registered ()
  :tags '(:deferred)
  (require 'yasnippet)
  (should (memq 'yasnippet-snippets-dir yas-snippet-dirs)))

;; yas--subdirs / yas--table-* / yas--load-pending-jits は内部関数だが、
;; 「スニペットが実際に供給されるか」を検証する唯一の手段のためテスト側でのみ使う。
(ert-deftest my-test-deferred-yasnippet-no-leaf-dirs ()
  :tags '(:deferred)
  ;; モード別のリーフディレクトリ (.../snippets/c-mode) を yas-snippet-dirs へ
  ;; 直接入れると、yas-load-directory はその直下をモード名として走査するため
  ;; スニペットが 1 件も供給されない。root 以外の配下指定を禁じる。
  (require 'yasnippet)
  (let ((root (file-name-as-directory (symbol-value 'yasnippet-snippets-dir))))
    (should (yas--subdirs root))
    (dolist (dir (yas-snippet-dirs))
      (should-not (and (not (equal (file-name-as-directory dir) root))
                       (file-in-directory-p dir root))))))

(ert-deftest my-test-deferred-yasnippet-c-mode-snippets-available ()
  :tags '(:deferred)
  (require 'yasnippet)
  ;; with-temp-buffer のバッファ名は先頭が空白のため yas-global-mode は自動適用しない。
  (with-temp-buffer
    (c-mode)
    (yas-minor-mode 1)
    (yas--load-pending-jits)
    (should (memq 'c-mode (mapcar #'yas--table-mode (yas--get-snippet-tables))))
    (should (> (hash-table-count (yas--table-hash (yas--table-get-create 'c-mode))) 0))))

(ert-deftest my-test-deferred-yasnippet-no-fictional-mode ()
  :tags '(:deferred)
  ;; custom/snippets を無条件に top-level dir として登録すると、直下の
  ;; snippets ディレクトリ名がメジャーモードとして intern され、架空モード
  ;; 'snippets が登録される。
  (require 'yasnippet)
  (should-not (gethash 'snippets yas--scheduled-jit-loads))
  (should-not (gethash 'snippets yas--tables)))

(ert-deftest my-test-deferred-yasnippet-personal-dirs-layouts ()
  :tags '(:deferred)
  ;; 混在時に意図的に警告を出すため、ハーネスの記録を汚さないよう let-bind する
  (let ((my-test--recorded-warnings nil)
        (root (make-temp-file "my-test-yas-" t)))
    (unwind-protect
        (let ((linked (expand-file-name "snippets" root)))
          ;; 空 root: 何も返さない
          (should-not (my/yas-personal-snippet-dirs root))
          ;; レイアウト A のみ: root を返す
          (make-directory (expand-file-name "c-mode" root) t)
          (should (equal (my/yas-personal-snippet-dirs root) (list root)))
          (should-not my-test--recorded-warnings)
          ;; レイアウト A+B 混在: 非対応構成として linked のみ + 警告 1 件
          (make-directory (expand-file-name "snippets/c-mode" root) t)
          (should (equal (my/yas-personal-snippet-dirs root) (list linked)))
          (should (= (length my-test--recorded-warnings) 1))
          ;; レイアウト B のみ: linked を返す (警告なし)
          (delete-directory (expand-file-name "c-mode" root) t)
          (setq my-test--recorded-warnings nil)
          (should (equal (my/yas-personal-snippet-dirs root) (list linked)))
          (should-not my-test--recorded-warnings))
      (delete-directory root t))))

(ert-deftest my-test-deferred-yasnippet-mixed-layout-loads-no-fictional-mode ()
  :tags '(:deferred)
  ;; 返却リストだけでなく、実際に yasnippet がロードした結果にも架空モードが
  ;; 現れないことを検証する。
  (require 'yasnippet)
  (let ((my-test--recorded-warnings nil)
        (root (make-temp-file "my-test-yas-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "c-mode" root) t)
          (make-directory (expand-file-name "snippets/c-mode" root) t)
          (let ((yas-snippet-dirs (my/yas-personal-snippet-dirs root)))
            (yas-reload-all)
            (should-not (gethash 'snippets yas--scheduled-jit-loads))
            (should-not (gethash 'snippets yas--tables))))
      (delete-directory root t)
      ;; let を抜けた本来の yas-snippet-dirs でグローバル状態を復元する
      (yas-reload-all))))

(ert-deftest my-test-deferred-yasnippet-resolve-drops-invalid ()
  :tags '(:deferred)
  ;; 未 bound シンボル・非文字列値・不存在ディレクトリを、型エラーを起こさずに
  ;; 警告付きで除去する。file-directory-p を先に呼ぶと非文字列値で
  ;; wrong-type-argument になる。
  (let ((my-test--recorded-warnings nil)
        (valid (my-set-custom "")))
    (should (equal (my/yas-resolve-snippet-dirs
                    (list valid
                          'my-test-definitely-unbound-symbol
                          'my-test-deferred--non-string-value
                          (expand-file-name "no-such-dir" valid)))
                   (list valid)))
    (should (= (length my-test--recorded-warnings) 3))))

(ert-deftest my-test-deferred-yasnippet-setup-degrades-when-all-invalid ()
  :tags '(:deferred)
  ;; yas-snippet-dirs が nil になると yas--load-snippet-dirs が
  ;; (call-interactively 'yas-load-directory) を呼び、起動が対話プロンプトで止まる。
  ;; その経路へ入らず、警告を出して縮退することを検証する。
  (require 'yasnippet)
  (let ((my-test--recorded-warnings nil)
        (yas-snippet-dirs (list 'my-test-definitely-unbound-symbol "/no/such/dir")))
    (should-not (my/yas-setup))          ; yas-global-mode を有効化しない
    (should-not yas-snippet-dirs)        ; let-bind のため実環境へ影響しない
    (should (= (length my-test--recorded-warnings) 3))) ; 除外 2 件 + 縮退 1 件
  ;; グローバルの yas-global-mode は上記に影響されず有効なまま
  (should (default-value 'yas-global-mode)))

(ert-deftest my-test-deferred-yasnippet-no-default-user-dir ()
  :tags '(:deferred)
  ;; early-init.el が yas-snippet-dirs を先に設定して ~/.emacs.d/snippets/ の
  ;; 自動生成を防いでいる。その効果が失われていないことを結果側で担保する。
  (require 'yasnippet)
  (should-not (member yas--default-user-snippets-dir (yas-snippet-dirs)))
  (should-not (file-directory-p yas--default-user-snippets-dir)))

;;; 遅延ロード時の警告
;; 遅延 :config が出す警告は my-test-startup.el のロード時点では未発生のため、
;; 起動検査を素通りする。require 後にここで検査する。
(ert-deftest my-test-deferred-no-unexpected-warnings ()
  :tags '(:deferred)
  (dolist (entry my-test-deferred--feature-modes)
    (require (car entry)))
  (should-not (my-test-startup-check-warnings)))

(provide 'my-test-deferred)
;;; my-test-deferred.el ends here
