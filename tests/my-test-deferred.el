;;; my-test-deferred.el --- 遅延ロード設定の回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; :defer N で遅延される use-package の :config が正しく動くことを検証する。
;; バッチでは idle timer が発火しないため、require で :config を発火させ、
;; 対応するグローバルモードが有効になることを確認する。
;; 注意: このテストは対象 feature をロードするため、未ロードを検証する
;; my-test-packages.el (:invariant) とは別プロセス (test-deferred) で実行する。

;;; Code:

(require 'ert)

;;;;; [Group] Deferred - 遅延ロード後のグローバルモード有効化 ;;;;;
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

;;;;; [Group] Deferred - yasnippet のスニペットディレクトリ ;;;;;
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

;;;;; [Group] Deferred - 遅延ロード時の警告 ;;;;;
;; 遅延 :config が出す警告は my-test-startup.el のロード時点では未発生のため、
;; 起動検査を素通りする。require 後にここで検査する。
(ert-deftest my-test-deferred-no-unexpected-warnings ()
  :tags '(:deferred)
  (dolist (entry my-test-deferred--feature-modes)
    (require (car entry)))
  (should-not (my-test-startup-check-warnings)))

(provide 'my-test-deferred)
;;; my-test-deferred.el ends here
