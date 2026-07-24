;;; my-test-global-modes.el --- グローバルモードの回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; 起動後に有効化されるグローバルモードのフック登録を検証する。

;;; Code:

(require 'ert)

;;;;; [Group] Global Modes - after-init-hook ;;;;;
;; GUI 限定ガード付きの宣言（pixel-scroll-precision-mode 等）は
;; バッチでは評価されないため対象外とする。
(defconst my-test-global-modes--after-init-functions
  '(which-key-mode
    global-anzu-mode
    recentf-mode
    global-total-lines-mode
    dashboard-setup-startup-hook
    my/global-capf
    mode-line-bell-mode
    repeat-mode
    global-so-long-mode
    tab-bar-mode
    windmove-default-keybindings
    delete-selection-mode
    auto-save-visited-mode
    global-auto-revert-mode
    save-place-mode
    savehist-mode
    electric-pair-mode
    global-whitespace-mode
    display-time-mode
    doom-modeline-mode)
  "after-init-hook に登録されているべき関数。")

(ert-deftest my-test-global-modes-after-init-hook-membership ()
  :tags '(:invariant)
  (dolist (function my-test-global-modes--after-init-functions)
    (should (memq function after-init-hook))))

;;;;; [Group] Global Modes - デフォルト有効モード ;;;;;
;; Emacs 28+ でデフォルト有効のモードは hook 登録ではなく有効状態を検証する
(ert-deftest my-test-global-modes-show-paren-enabled ()
  :tags '(:invariant)
  (should (default-value 'show-paren-mode)))

(provide 'my-test-global-modes)
;;; my-test-global-modes.el ends here
