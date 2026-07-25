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

(provide 'my-test-deferred)
;;; my-test-deferred.el ends here
