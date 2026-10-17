;;; my-test-keybindings.el --- タグ操作キーバインドの回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; 基本操作、C/C++ タグナビゲーション、パッケージ固有のキーを検証する。

;;; Code:

(require 'ert)
(require 'ggtags)

;;; 操作を探しながら覚えるヘルプ
(ert-deftest my-test-keybindings-learning-entry ()
  :tags '(:keybinding)
  (dolist (entry '(("h" . my/key-guide) ("s" . my/command-palette)
                   ("k" . describe-key) ("b" . describe-bindings)
                   ("m" . describe-mode) ("t" . help-with-tutorial)))
    (should (eq (keymap-lookup global-map (concat "C-c h " (car entry)))
                (cdr entry)))))

(ert-deftest my-test-keybindings-learning-current-context ()
  "案内のキーはローカルな上書きとリマップを反映すること。"
  :tags '(:keybinding)
  (with-temp-buffer
    (let ((map (make-sparse-keymap)))
      (use-local-map map)
      (keymap-set map "C-s" #'ignore)
      (keymap-set map "C-c v" #'consult-line)
      (should (equal (my/key-guide--binding 'consult-line) "C-c v"))
      (define-key map [remap consult-line] #'forward-char)
      (let ((entry (seq-find (lambda (entry) (eq (cdr entry) 'forward-char))
                             (my/key-guide--candidates))))
        (should entry)
        (should (string-match-p "forward-char" (car entry)))))
    ;; この設定では C-h を押してもヘルプへ届かない。
    (should-not (string-match-p "C-h" (my/key-guide--binding 'describe-key)))))

(ert-deftest my-test-keybindings-learning-palette-dispatch ()
  "選んだ操作は元バッファの選択範囲と前置引数を受け取ること。"
  :tags '(:keybinding)
  (with-temp-buffer
    (insert "abc")
    (set-mark 1)
    (let ((origin (current-buffer))
          (my/key-guide-commands '(("コピー copy" . kill-ring-save)))
          (current-prefix-arg '(4))
          observed)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _) (caar collection)))
                ((symbol-function 'kill-ring-save)
                 (lambda (beg end &optional _region)
                   (interactive "r")
                   (setq observed (list (current-buffer) beg end current-prefix-arg)))))
        (call-interactively #'my/command-palette)
        (should (equal observed (list origin 1 4 '(4))))))))

(ert-deftest my-test-keybindings-learning-palette-cancel ()
  :tags '(:keybinding)
  (with-temp-buffer
    (insert "変更しない")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (signal 'quit nil))))
      (should (eq (condition-case nil (my/command-palette) (quit 'cancelled))
                  'cancelled)))
    (should (equal (buffer-string) "変更しない"))))

(ert-deftest my-test-keybindings-learning-terminal-keys ()
  "GUI の open/copy キーより、端末で入力できる文字キーを案内すること。"
  :tags '(:keybinding)
  (with-temp-buffer
    (should (equal (my/key-guide--binding 'find-file) "C-x C-f"))
    (should (equal (my/key-guide--binding 'kill-ring-save) "M-w"))
    (should-not (string-match-p "<" (my/key-guide--binding 'describe-key)))
    (let ((map (make-sparse-keymap)))
      (use-local-map map)
      (keymap-set map "M-w" #'ignore)
      (should (equal (my/key-guide--binding 'kill-ring-save)
                     "M-x kill-ring-save")))))

(ert-deftest my-test-keybindings-learning-cpp-scope ()
  :tags '(:keybinding)
  (with-temp-buffer
    (should-not (rassq 'my/gtags-find-definition (my/key-guide--candidates)))
    (c++-mode)
    (should (rassq 'my/gtags-find-definition (my/key-guide--candidates)))))

(ert-deftest my-test-keybindings-learning-guide-origin ()
  "ヘルプバッファではなく、呼び出し元のキーで一覧を作ること。"
  :tags '(:keybinding)
  (save-window-excursion
    (with-temp-buffer
      (let ((map (make-sparse-keymap)))
        (use-local-map map)
        (keymap-set map "C-s" #'ignore)
        (keymap-set map "C-c v" #'consult-line)
        (my/key-guide)
        (with-current-buffer "*操作ガイド*"
          (should (derived-mode-p 'help-mode))
          (should (string-match-p "C-c v" (buffer-string)))
          (should (string-match-p "C-g" (buffer-string))))))))

;;; 日本語の説明
(ert-deftest my-test-keybindings-japanese-global-coverage ()
  "標準を含む文字キーの割り当てに説明漏れがないこと。"
  :tags '(:keybinding)
  (let (missing seen)
    (dolist (entry (append my/key-guide-commands my/key-guide--cpp-commands
                          my/key-guide--extra-descriptions))
      (should-not (memq (cdr entry) seen))
      (push (cdr entry) seen))
    (dolist (pair (accessible-keymaps (current-global-map)))
      (when (seq-every-p #'integerp (car pair))
        (map-keymap
         (lambda (key binding)
           (when (and (integerp key) (symbolp binding) (commandp binding)
                      (not (my/key-guide-description binding)))
             (cl-pushnew binding missing)))
         (cdr pair))))
    (should-not missing)))

(ert-deftest my-test-keybindings-japanese-key-list ()
  "既存キー一覧の日本語が上書きされたコマンドにも追従すること。"
  :tags '(:keybinding)
  (save-window-excursion
    (with-temp-buffer
      (let ((map (make-sparse-keymap)))
        (keymap-set map "C-c z" #'save-buffer)
        (use-local-map map)
        (describe-bindings)
        (with-current-buffer (help-buffer)
          (goto-char (point-min))
          (should (re-search-forward "C-c z[ \t]+save-buffer.*ファイルを保存" nil t))
          (goto-char (point-min))
          (should (search-forward "save-buffer" nil t))
          ;; コマンド名のリンクと原文を残し、再表示で説明を重ねない。
          (should (button-at (1- (point))))
          (let ((before (buffer-string)))
            (my/key-guide-annotate-key-list)
            (should (equal before (buffer-string)))))
        (should (eq (key-binding (kbd "C-c z")) 'save-buffer))))))

(ert-deftest my-test-keybindings-japanese-which-key ()
  "日本語の説明がコマンドに追従し、未知の操作を誤訳しないこと。"
  :tags '(:keybinding)
  (require 'which-key)
  (dolist (entry '(("C-z n" "tab-next" "次のタブへ移動")
                   ("C-q n" "mc/mark-next-like-this" "次の一致箇所にカーソルを追加")
                   ("C-t e" "my/lsp-find-callees" "呼び出し先を探す")
                   ("C-c j j" "copilot-accept-completion" "補完を確定")
                   ("C-c J e" "copilot-chat-explain" "選択したコードを説明")))
    (should (equal (cdr (which-key--maybe-replace (cons (nth 0 entry) (nth 1 entry))))
                   (nth 2 entry))))
  ;; 同じキーに別の操作が割り当てられても、キーだけを見て誤訳しない。
  (should (equal (cdr (which-key--maybe-replace '("C-c J e" . "save-buffer")))
                 "ファイルを保存"))
  (should (equal (which-key--maybe-replace '("C-c z" . "unknown-operation"))
                 '("C-c z" . "unknown-operation"))))

(ert-deftest my-test-keybindings-which-key-group-commands ()
  "同一コマンドのキーをまとめ、別コマンド・prefix・非表示を区別すること。"
  :tags '(:keybinding)
  (require 'which-key)
  (let* ((which-key-sort-order nil)
         (which-key-replacement-alist
          '((("\\`h\\'" . nil) . t)
            ((nil . "\\`forward-char\\'") . (nil . "移動"))
            ((nil . "\\`backward-char\\'") . (nil . "移動"))))
         (map (make-sparse-keymap)))
    (dolist (key '("C-f" "f" "h"))
      (keymap-set map key #'forward-char))
    (keymap-set map "b" #'backward-char)
    (keymap-set map "x" (make-sparse-keymap))
    (keymap-set map "y" (make-sparse-keymap))
    (let ((rows (which-key--get-bindings nil map)))
      (should (equal (mapcar #'car rows) '("C-f/f" "b" "x" "y")))
      (should (equal (mapcar #'caddr (seq-take rows 2)) '("移動" "移動")))))
  ;; 全キー一覧では prefix を含むキー表記も欠落しない。
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-t e" #'my/lsp-find-callees)
    (keymap-set map "C-t C-e" #'my/lsp-find-callees)
    (should (equal (caar (which-key--get-bindings nil map nil t))
                   "C-t e/C-t C-e"))))

(ert-deftest my-test-keybindings-which-key-overridden-command ()
  "一方のキーが別コマンドに変われば、説明文が同じでも別々に表示すること。"
  :tags '(:keybinding)
  (require 'which-key)
  (with-temp-buffer
    (let ((map (make-sparse-keymap))
          (which-key-replacement-alist
           '(((nil . "\\`forward-char\\'") . (nil . "移動"))
             ((nil . "\\`backward-char\\'") . (nil . "移動")))))
      (keymap-set map "C-t e" #'forward-char)
      (keymap-set map "C-t C-e" #'forward-char)
      (use-local-map map)
      (should (= (length (which-key--get-bindings (kbd "C-t"))) 1))
      (keymap-set map "C-t C-e" #'backward-char)
      (let ((rows (which-key--get-bindings (kbd "C-t"))))
        (should (equal (mapcar #'car rows) '("e" "C-e")))
        (should (equal (mapcar #'caddr rows) '("移動" "移動")))))))

(ert-deftest my-test-keybindings-which-key-binding-types ()
  "コマンド名と同じ文字列のマクロやメニュー名を誤統合しないこと。"
  :tags '(:keybinding)
  (require 'which-key)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'forward-char)
    (define-key map (kbd "b") "forward-char")
    (define-key map (kbd "c") '("forward-char" . backward-char))
    (should (equal (mapcar #'car (which-key--get-bindings nil map))
                   '("a" "b" "c")))))

(ert-deftest my-test-keybindings-japanese-mode-help ()
  "モード固有のキー一覧にも日本語の説明を添えること。"
  :tags '(:keybinding)
  (save-window-excursion
    (with-temp-buffer
      (org-mode)
      (describe-mode)
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (should (re-search-forward "org-todo.*タスクの状態を変更" nil t))))))

(ert-deftest my-test-keybindings-japanese-marginalia ()
  "M-x の説明を日本語にしつつキー表示と未翻訳時の説明を保つこと。"
  :tags '(:keybinding)
  (require 'marginalia)
  (should (eq (cadr (assq 'command marginalia-annotators))
              'my/key-guide-annotate-command))
  (should (eq (cadr (assq 'function marginalia-annotators))
              'my/key-guide-annotate-function))
  (should (string-match-p "タブを新規作成"
                          (my/key-guide-annotate-function "tab-new")))
  (let* ((original (marginalia-annotate-function "save-buffer"))
         (translated (my/key-guide-annotate-function "save-buffer")))
    ;; 元の種別・引数・原文概要と装飾をそのまま保持する。
    (should (string-prefix-p "ファイルを保存" translated))
    (should (equal-including-properties
             original (substring translated (- (length original))))))
  (let ((annotation (my/key-guide-annotate-command "tab-new")))
    (should (string-match-p "タブを新規作成" annotation))
    (should (string-prefix-p (marginalia-annotate-binding "tab-new") annotation)))
  (cl-letf (((symbol-function 'marginalia-annotate-command)
             (lambda (candidate) (concat "元の説明: " candidate))))
    (should (equal (my/key-guide-annotate-command "unknown-operation")
                   "元の説明: unknown-operation"))))

(ert-deftest my-test-keybindings-japanese-magit-menu ()
  "Git の入口を日本語にしても、実行先とキーを維持すること。"
  :tags '(:keybinding)
  (require 'magit)
  (let ((suffix (transient-get-suffix 'magit-dispatch "c")))
    (should (eq (plist-get (cdr suffix) :command) 'magit-commit))
    (should (equal (plist-get (cdr suffix) :description) "コミット")))
  (let ((suffix (transient-get-suffix 'magit-dispatch "S")))
    (should (eq (plist-get (cdr suffix) :command) 'magit-stage-modified))
    (should (string-match-p "追跡中" (plist-get (cdr suffix) :description)))
    (should (string-match-p "追跡中" (my/key-guide-description 'magit-stage-modified)))))

(ert-deftest my-test-keybindings-japanese-submenus ()
  "固定説明を補い、実行先・Git 引数と動的対象の説明を保つこと。"
  :tags '(:keybinding)
  (require 'magit)
  (require 'copilot-chat-transient)
  (dolist (entry '((magit-diff "u" magit-diff-unstaged)
                   (magit-diff-refresh "t" magit-diff-toggle-refine-hunk)
                   (magit-commit "c" magit-commit-create)
                   (magit-branch "n" magit-branch-create)
                   (magit-push "e" magit-push-current)
                   (magit-pull "e" magit-pull-branch)
                   (copilot-chat-transient "S" copilot-chat-save)
                   (copilot-chat-transient-buffers "D" copilot-chat-list-clear-buffers)
                   (copilot-chat-transient-buffers "c" copilot-chat-list-clear-buffers)
                   (copilot-chat-transient-code "E" copilot-chat-explain-symbol-at-line)))
    (pcase-let* ((`(,prefix ,key ,command) entry)
                 (props (cdr (transient-get-suffix prefix key))))
      (should (eq (plist-get props :command) command))
      (should (equal (plist-get props :key) key))
      (should (string-match-p "[ぁ-んァ-ヶ一-龠]" (plist-get props :description)))))
  (dolist (entry '((magit-diff "-w" "--ignore-all-space")
                   (magit-commit "-n" "--no-verify")
                   (magit-push "-F" "--force")
                   (magit-pull "-f" "--ff-only")))
    (pcase-let* ((`(,prefix ,key ,argument) entry)
                 (props (cdr (transient-get-suffix prefix key))))
      (should (equal (plist-get props :argument) argument))
      (should (string-match-p "[ぁ-んァ-ヶ一-龠]" (plist-get props :description)))))
  ;; 対象名と未設定・不正な設定の警告は upstream の関数に委ねる。
  (dolist (entry '((magit-push "p" magit-push--pushbranch-description)
                   (magit-push "u" magit-push--upstream-description)
                   (magit-pull "p" magit-pull--pushbranch-description)
                   (magit-pull "u" magit-pull--upstream-description)))
    (pcase-let* ((`(,prefix ,key ,description) entry)
                 (props (cdr (transient-get-suffix prefix key)))
                 (command (plist-get props :command)))
      (should-not (plist-member props :description))
      (should (eq (oref (get command 'transient--suffix) description) description))))
  (dolist (entry '((magit-branch (0)) (magit-branch-configure (0))
                   (magit-push (1)) (magit-pull (0)) (magit-pull (1))))
    (should (functionp (plist-get (aref (transient-get-suffix (car entry) (cadr entry)) 1)
                                 :description)))))

(ert-deftest my-test-keybindings-japanese-mode-operations ()
  "日常のモード固有キーから到達する操作を共通辞書で案内すること。"
  :tags '(:keybinding)
  (require 'magit)
  (require 'git-commit)
  (require 'copilot-chat-prompt-mode)
  (require 'copilot-chat-command)
  (dolist (entry '((magit-mode-map "+" magit-diff-more-context)
                   (magit-mode-map "-" magit-diff-less-context)
                   (magit-mode-map "0" magit-diff-default-context)
                   (magit-diff-mode-map "j" magit-jump-to-diffstat-or-diff)
                   (magit-diff-mode-map "C-c C-b" magit-go-backward)
                   (magit-diff-mode-map "C-c C-f" magit-go-forward)
                   (magit-section-mode-map "^" magit-section-up)
                   (magit-section-mode-map "M-p" magit-section-backward-sibling)
                   (magit-section-mode-map "M-n" magit-section-forward-sibling)
                   (git-commit-mode-map "M-p" git-commit-prev-message)
                   (git-commit-mode-map "M-n" git-commit-next-message)
                   (git-commit-mode-map "C-c C-i" git-commit-insert-trailer)
                   (git-commit-mode-map "C-c C-d" magit-diff-while-committing)
                   (copilot-chat-prompt-mode-map "C-c C-c" copilot-chat-prompt-send)
                   (copilot-chat-prompt-mode-map "C-c C-l" copilot-chat-prompt-split-and-list)
                   (copilot-chat-prompt-mode-map "M-p" copilot-chat-prompt-history-previous)
                   (copilot-chat-prompt-mode-map "M-n" copilot-chat-prompt-history-next)
                   (copilot-chat-list-mode-map "RET" copilot-chat-list-add-or-remove-buffer)
                   (copilot-chat-list-mode-map "C-c C-c" copilot-chat-list-clear-buffers)))
    (pcase-let ((`(,map ,key ,command) entry))
      (with-temp-buffer
        (use-local-map (symbol-value map))
        (should (eq (key-binding (kbd key)) command)))
      (should (my/key-guide-description command))))
  ;; 実モードのヘルプにも辞書が届き、原文のコマンド名は残る。
  (with-temp-buffer
    (copilot-chat-prompt-mode 1)
    (save-window-excursion
      (describe-mode)
      (with-current-buffer (help-buffer)
        (should (string-match-p "copilot-chat-prompt-send.*質問を送信" (buffer-string)))))))

(ert-deftest my-test-keybindings-japanese-help-summary ()
  "標準ヘルプの先頭に日本語を加え、原文とキーの動作を残すこと。"
  :tags '(:keybinding)
  (let ((original (documentation 'save-buffer)))
    (save-window-excursion
      (describe-key (kbd "C-x C-s"))
      (with-current-buffer (help-buffer)
        (should (string-prefix-p "日本語の説明: ファイルを保存" (buffer-string)))
        (should (string-match-p "save-buffer" (buffer-string))))
      (describe-function 'save-buffer)
      (with-current-buffer (help-buffer)
        (should (string-prefix-p "日本語の説明: ファイルを保存" (buffer-string)))
        (should (string-match-p (regexp-quote (car (split-string original "\n")))
                                (buffer-string)))))
    (should (equal (documentation 'save-buffer) original))
    (should (eq (keymap-lookup global-map "C-x C-s") 'save-buffer))))

;;; 基本操作と日本語入力
(ert-deftest my-test-keybindings-basic-bindings ()
  "設定の配置を変えても、日常の基本操作を同じキーで呼べること。"
  :tags '(:keybinding)
  (dolist (binding '(("C-c a" . align)
                     ("C-c M-a" . align-regexp)
                     ("C-c d" . delete-indentation)
                     ("M-?" . help-for-help)
                     ("C-c C-i" . hippie-expand)
                     ("C-c ;" . comment-or-uncomment-region)
                     ("M-f" . forward-symbol)
                     ("<f7>" . toggle-truncate-lines)
                     ("C-c 0" . my/copy-file-name)
                     ("C-c C-r" . my/window-resizer)
                     ("M-p" . my/other-window-or-split)
                     ("<zenkaku-hankaku>" . toggle-input-method)
                     ("<muhenkan>" . my/input-method-off)
                     ("<henkan>" . my/input-method-on)))
    (should (eq (keymap-lookup global-map (car binding)) (cdr binding))))
  ;; 端末で C-h がヘルプへ戻らず、従来どおり DEL に変換されること。
  (should (equal (key-description (lookup-key key-translation-map (kbd "C-h")))
                 "DEL")))

;;; タグナビゲーション
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

;;; 折りたたみ・見出し
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

;;; シンボル操作
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

;;; 複数カーソル
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

;;; 検索
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

;;; タブ操作
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

;;; パッケージ固有キーマップ
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
