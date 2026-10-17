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
    update-gtags
    my/lsp-find-callers
    my/lsp-find-callees
    my/lsp-find-type-hierarchy
    my/lsp-find-other-file
    my/lsp-find-workspace-symbol
    my/outline-cycle-dwim
    my/eglot-inlay-hints-toggle)
  "定義済みでなければならないタグ操作コマンド。")

;; C-t 配下へ追加した読解補助のキー。固定表 (my-test-keybindings--bindings) の
;; 対象外だが、追加後の退行を防ぐため同じ方法で固定する。
(defconst my-test-keybindings--ct-extra-bindings
  '(("C-t c" . my/lsp-find-callers)
    ("C-t C-c" . my/lsp-find-callers)
    ("C-t e" . my/lsp-find-callees)
    ("C-t C-e" . my/lsp-find-callees)
    ("C-t t" . my/lsp-find-type-hierarchy)
    ("C-t C-t" . my/lsp-find-type-hierarchy)
    ("C-t o" . my/lsp-find-other-file)
    ("C-t C-o" . my/lsp-find-other-file)
    ("C-t s" . my/lsp-find-workspace-symbol)
    ("C-t C-s" . my/lsp-find-workspace-symbol)
    ("C-t l" . my/eglot-inlay-hints-toggle)
    ("C-t C-l" . my/eglot-inlay-hints-toggle))
  "ggtags キーマップへ追加した読解補助のキーバインド。")

(ert-deftest my-test-keybindings-ct-bindings ()
  :tags '(:keybinding)
  (dolist (binding my-test-keybindings--bindings)
    (should (eq (lookup-key ggtags-mode-map (kbd (car binding)))
                (cdr binding)))))

(ert-deftest my-test-keybindings-ct-extra-bindings ()
  :tags '(:keybinding)
  (dolist (binding my-test-keybindings--ct-extra-bindings)
    (should (eq (lookup-key ggtags-mode-map (kbd (car binding)))
                (cdr binding)))))

(ert-deftest my-test-keybindings-command-definitions ()
  :tags '(:keybinding)
  (dolist (command my-test-keybindings--commands)
    (should (fboundp command))))

;;;;; [Group] Keybinding - 折りたたみ・見出し ;;;;;
;; <f9> は outline が有効な C/C++ ts モードでは関数単位の折りたたみ、それ以外では
;; 従来の hs-toggle-hiding。既存の <f5> / C-\ は変更しない。
(defconst my-test-keybindings--folding-bindings
  '(([f9]    . my/outline-cycle-dwim)
    ("M-g o" . consult-outline)
    ([f5]    . hs-toggle-hiding)
    ("C-\\"  . hs-toggle-hiding))
  "固定する折りたたみ・見出し移動のグローバルキーバインド。")

(ert-deftest my-test-keybindings-folding-bindings ()
  :tags '(:keybinding)
  (dolist (binding my-test-keybindings--folding-bindings)
    (let ((key (if (stringp (car binding)) (kbd (car binding)) (car binding))))
      (should (eq (lookup-key global-map key) (cdr binding)))))
  ;; M-s o は Emacs 標準の occur のまま (consult-outline で奪わない)
  (should (eq (lookup-key global-map (kbd "M-s o")) 'occur)))

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

;;;;; [Group] Keybinding - タブ操作 ;;;;;
;; C-z はタブ操作のプレフィックス。C-z 1〜9 は引数なしの tab-bar-select-tab が
;; 押した数字キー (last-command-event) からタブ番号を読む前提で同じコマンドを共有する。
(defconst my-test-keybindings--tab-bar-bindings
  `(("C-z n"   . tab-next)
    ("C-z C-n" . tab-next)
    ("C-z p"   . tab-previous)
    ("C-z C-p" . tab-previous)
    ("C-z f"   . tab-new)
    ("C-z C-f" . tab-new)
    ("C-z k"   . tab-close)
    ("C-z C-k" . tab-close)
    ,@(mapcar (lambda (n) (cons (format "C-z %d" n) 'tab-bar-select-tab))
              '(1 2 3 4 5 6 7 8 9)))
  "固定する C-z プレフィックスのタブ操作キーバインド。")

(ert-deftest my-test-keybindings-tab-bar-bindings ()
  :tags '(:keybinding)
  (should (keymapp (lookup-key global-map (kbd "C-z"))))
  (dolist (binding my-test-keybindings--tab-bar-bindings)
    (should (eq (lookup-key global-map (kbd (car binding)))
                (cdr binding)))))

(ert-deftest my-test-keybindings-tab-bar-select-by-digit ()
  "C-z N が N 番目のタブへ移動すること (数字キーからの番号解決)。"
  :tags '(:keybinding)
  (dotimes (_ 8) (tab-bar-new-tab))
  (unwind-protect
      (progn
        (should (= (length (tab-bar-tabs)) 9))
        (dolist (n '(3 1 9 5))
          (let ((last-command-event (+ ?0 n))
                (current-prefix-arg nil))
            (call-interactively
             (lookup-key global-map (kbd (format "C-z %d" n)))))
          (should (= (1+ (tab-bar--current-tab-index)) n))))
    (tab-bar-close-other-tabs)))

;;;;; [Group] Keybinding - パッケージ固有キーマップ ;;;;;
;; :bind :map で宣言したキーは、パッケージのロード後に eval-after-load 経由で入る。
;; 宣言の移し替えで束縛が消えても起動検査では気づけないため、ロード後の状態を固定する。
(defconst my-test-keybindings--package-map-bindings
  '((vundo vundo-mode-map ("C-f" . vundo-forward) ("C-b" . vundo-backward)
           ("C-n" . vundo-next) ("C-p" . vundo-previous))
    (magit magit-status-mode-map ("q" . mu-magit-kill-buffers))
    (markdown-mode markdown-mode-map ("C-c TAB" . my-markdown-insert-tab)
                   ("C-c C-c e" . my-markdown-export)))
  "(FEATURE KEYMAP (KEY . COMMAND)...) の形で固定するパッケージ固有の束縛。")

(ert-deftest my-test-keybindings-package-map-bindings ()
  :tags '(:keybinding)
  (dolist (entry my-test-keybindings--package-map-bindings)
    (require (car entry))
    (let ((map (symbol-value (nth 1 entry))))
      (dolist (binding (nthcdr 2 entry))
        (should (eq (keymap-lookup map (car binding)) (cdr binding)))))))

;; gfm-mode は markdown-mode-map を親に持つため、C-c TAB の上書きが GFM 側でも効くこと
(ert-deftest my-test-keybindings-markdown-tab-in-gfm ()
  :tags '(:keybinding)
  (require 'markdown-mode)
  (with-temp-buffer
    (gfm-mode)
    (should (eq (keymap-lookup nil "C-c TAB") 'my-markdown-insert-tab))
    (should (eq (keymap-lookup nil "C-c C-c e") 'my-markdown-export))))

(provide 'my-test-keybindings)
;;; my-test-keybindings.el ends here
