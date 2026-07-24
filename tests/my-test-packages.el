;;; my-test-packages.el --- パッケージロードの回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; 削除済みパッケージと遅延ロード対象の feature 状態を検証する。

;;; Code:

(require 'ert)

;;;;; [Group] Packages - 削除済み feature ;;;;;
;; 今後の削除フェーズでは、削除したパッケージをこの表へ追記する。
(defconst my-test-packages--banned-features
  '()
  "起動直後にロードされていてはならない削除済み feature。")

(ert-deftest my-test-packages-banned-features ()
  :tags '(:invariant)
  (dolist (feature my-test-packages--banned-features)
    (should-not (featurep feature))))

;;;;; [Group] Packages - eager-load 回帰検知 ;;;;;
;; undo-fu は現状 eager ロードのため含めず、後続フェーズで遅延化後に追加する。
(defconst my-test-packages--deferred-features
  '(irony
    yasnippet
    magit
    org
    tramp
    smart-mode-line
    consult
    which-key
    undo-fu
    doom-modeline)
  "フル起動直後に遅延ロード状態でなければならない feature。")

(ert-deftest my-test-packages-deferred-features ()
  :tags '(:invariant)
  (dolist (feature my-test-packages--deferred-features)
    (should-not (featurep feature))))

;;;;; [Group] Packages - ライブラリ解決先 ;;;;;
(defun my-test-packages--straight-build-p (library)
  "LIBRARY の解決先が straight/build 配下なら non-nil を返す。"
  (let ((path (locate-library library)))
    (and path
         (file-in-directory-p path (my-set-straight "build/")))))

(ert-deftest my-test-packages-which-key-built-in ()
  :tags '(:invariant)
  (should-not (my-test-packages--straight-build-p "which-key")))

(provide 'my-test-packages)
;;; my-test-packages.el ends here
