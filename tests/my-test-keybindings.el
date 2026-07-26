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

;;;;; [Group] Keybinding - シンボル操作 ;;;;;
;; highlight-symbol 廃止後も symbol-overlay でハイライトと置換が揃っていること
(defconst my-test-keybindings--symbol-overlay-bindings
  '(([f3]      . symbol-overlay-put)
    ([f4]      . symbol-overlay-remove-all)
    ("C-x C-a" . my-symbol-overlay-rename-visible)
    ("C-x a"   . my-symbol-overlay-rename-in-function)
    ("C-x C-g" . symbol-overlay-rename))
  "固定する symbol-overlay 系のグローバルキーバインド。")

(ert-deftest my-test-keybindings-symbol-overlay-bindings ()
  :tags '(:keybinding)
  (dolist (binding my-test-keybindings--symbol-overlay-bindings)
    (let ((key (if (stringp (car binding)) (kbd (car binding)) (car binding))))
      (should (eq (lookup-key global-map key) (cdr binding))))))

;;;;; [Group] Keybinding - 複数カーソル ;;;;;
;; multiple-cursors は遅延ロードするため、repeat-map と C-q プレフィックスは
;; パッケージ本体をロードせずに使える状態でなければならない。
;; mc/* は autoload 経由で解決されるので、束縛の存在だけを固定する。
(ert-deftest my-test-keybindings-mc-repeat-map ()
  :tags '(:keybinding)
  (should (boundp 'my/mc-repeat-map))
  (should (keymapp my/mc-repeat-map))
  (should (eq (lookup-key global-map (kbd "C-q")) my/mc-repeat-map))
  (dolist (key '("n" "p" "a" "d" "u" "s" "i" "l" "o"))
    (should (commandp (lookup-key my/mc-repeat-map (kbd key))))))

;;;;; [Group] Keybinding - 検索 ;;;;;
;; Emacs では C-S は C-s と同一のキーイベントであるため、両方を :bind へ並べると
;; 後勝ちで一方が到達不能になる。バッファ内検索と横断検索が別々のキーから
;; 呼べる状態を固定する（横断検索は端末が送出できる M-s l へ置く）。
(defconst my-test-keybindings--search-bindings
  '(("C-s"   . consult-line)
    ("M-s l" . my/consult-line-multi))
  "固定する consult 検索系のグローバルキーバインド。")

(ert-deftest my-test-keybindings-consult-search-bindings ()
  :tags '(:keybinding)
  ;; 退行の原因になった Emacs の仕様。C-S は独立したキーとして使えない
  (should (equal (kbd "C-s") (kbd "C-S")))
  (dolist (binding my-test-keybindings--search-bindings)
    (should (eq (lookup-key global-map (kbd (car binding)))
                (cdr binding))))
  ;; 2 つの検索が同じコマンドへ潰れていないこと
  (should-not (eq (lookup-key global-map (kbd "C-s"))
                  (lookup-key global-map (kbd "M-s l")))))

(provide 'my-test-keybindings)
;;; my-test-keybindings.el ends here
