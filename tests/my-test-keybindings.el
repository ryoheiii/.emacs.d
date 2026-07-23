;;; my-test-keybindings.el --- タグ操作キーバインドの回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; C/C++ タグナビゲーションの固定キーバインドと関数定義を検証する。

;;; Code:

(require 'ert)
(require 'ggtags)

;;;;; [Group] Keybinding - タグナビゲーション ;;;;;
(defconst my-test-keybindings--bindings
  '(("C-t d" . my/gtags-find-definition)
    ("C-t C-d" . my/gtags-find-definition)
    ("C-t u" . my/gtags-find-references)
    ("C-t C-u" . my/gtags-find-references)
    ("C-t v" . my/gtags-find-symbol)
    ("C-t C-v" . my/gtags-find-symbol)
    ("C-t f" . my/gtags-find-file)
    ("C-t C-f" . my/gtags-find-file)
    ("C-t p" . xref-go-back)
    ("C-t C-p" . xref-go-back)
    ("C-t n" . xref-go-forward)
    ("C-t C-n" . xref-go-forward))
  "固定する ggtags キーマップのキーバインド。")

(defconst my-test-keybindings--commands
  '(my/gtags-find-definition
    my/gtags-find-references
    my/gtags-find-symbol
    my/gtags-find-file
    xref-go-back
    xref-go-forward
    update-gtags)
  "定義済みでなければならないタグ操作コマンド。")

(ert-deftest my-test-keybindings-ct-bindings ()
  :tags '(:keybinding)
  (dolist (binding my-test-keybindings--bindings)
    (should (eq (lookup-key ggtags-mode-map (kbd (car binding)))
                (cdr binding)))))

(ert-deftest my-test-keybindings-command-definitions ()
  :tags '(:keybinding)
  (dolist (command my-test-keybindings--commands)
    (should (fboundp command))))

(provide 'my-test-keybindings)
;;; my-test-keybindings.el ends here
