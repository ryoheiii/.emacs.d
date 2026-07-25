;;; my-test-packages.el --- パッケージロードの回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; 削除済みパッケージと遅延ロード対象の feature 状態を検証する。

;;; Code:

(require 'ert)

;;;;; [Group] Packages - 削除済み feature ;;;;;
;; 今後の削除フェーズでは、削除したパッケージをこの表へ追記する。
(defconst my-test-packages--banned-features
  '(paradox amx smooth-scroll affe
    diminish smart-mode-line volatile-highlights highlight-symbol
    undo-fu flyspell-correct-popup)
  "起動直後にロードされていてはならない削除済み feature。")

(ert-deftest my-test-packages-banned-features ()
  :tags '(:invariant)
  (dolist (feature my-test-packages--banned-features)
    (should-not (featurep feature))))

;;;;; [Group] Packages - eager-load 回帰検知 ;;;;;
(defconst my-test-packages--deferred-features
  '(irony
    yasnippet
    magit
    org
    tramp
    consult
    which-key
    doom-modeline)
  "フル起動直後に遅延ロード状態でなければならない feature。")

(ert-deftest my-test-packages-deferred-features ()
  :tags '(:invariant)
  (dolist (feature my-test-packages--deferred-features)
    (should-not (featurep feature))))

;;;;; [Group] Packages - 起動時ブロッキング ;;;;;
;; find-at-startup は loads/straight/repos 配下を同期走査するため、起動が
;; リポジトリ規模とページキャッシュの温度に比例してブロックする。
;; 代替の検出経路まで含めて固定しないと、find-at-startup を外しただけで
;; 変更が一切検出されない状態でもテストが緑のままになる。
(ert-deftest my-test-packages-no-find-at-startup ()
  :tags '(:invariant)
  (should (boundp 'straight-check-for-modifications))
  (should-not (memq 'find-at-startup straight-check-for-modifications))
  ;; 代替の自動検出 (保存時フック) が設定されていること
  (should (memq 'check-on-save straight-check-for-modifications))
  ;; 手動チェック手段 (M-x straight-check-all) を失っていないこと
  (should (memq 'find-when-checking straight-check-for-modifications)))

;; check-on-save は bootstrap.el が straight-live-modifications-mode を
;; 有効化して初めて機能する。bootstrap より後で設定しても無効になるため、
;; 変数値だけでなくモードの実効状態を検査する。
(ert-deftest my-test-packages-live-modifications-enabled ()
  :tags '(:invariant)
  (should (bound-and-true-p straight-live-modifications-mode))
  (should (memq #'straight-register-file-modification
                (default-value 'before-save-hook))))

;;;;; [Group] Packages - ライブラリ解決先 ;;;;;
(defun my-test-packages--straight-build-p (library)
  "LIBRARY の解決先が straight/build 配下なら non-nil を返す。"
  (let ((path (locate-library library)))
    (and path
         (file-in-directory-p path (my-set-straight "build/")))))

(ert-deftest my-test-packages-which-key-built-in ()
  :tags '(:invariant)
  ;; 存在しない場合も straight-build-p は nil を返すため、実在を先に検証する
  (should (locate-library "which-key"))
  (require 'package)
  (should (package-built-in-p 'which-key))
  (should-not (my-test-packages--straight-build-p "which-key")))

(provide 'my-test-packages)
;;; my-test-packages.el ends here
