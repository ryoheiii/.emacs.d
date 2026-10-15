;;; my-test-tty-live.el --- 実 pty での tty 起動回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; shim と `--init-directory` により、実起動と同一のライフサイクルで検証する。
;; shim はパッケージ配置先、生成物の隔離、警告と after-init の観測を設定する。
;; 実 early-init 本体は無改変のまま load する。
;; 検査対象のフック面と宣言は依存パッケージ更新で変わり得る可変集合である。
;; 失敗時は設定の不具合と断定する前に、まず依存側の変化を確認して切り分ける。
;; 起動宣言に加え、データを読み書きする主要操作を固定fixtureで検査する。

;;; Code:

(require 'ert)
(require 'my-test-keybindings)
(require 'my-test-deferred)
(require 'my-test-startup)
(require 'my-gtags)
(require 'my-lsp-nav)

(when noninteractive
  (error "my-test-tty-live は実 pty 専用"))

;;;;; [Group] TTY Live - 起動ライフサイクル ;;;;;
(ert-deftest my-test-tty-live-init-loader-errors-empty ()
  :tags '(:tty-live)
  (should (string-empty-p (init-loader-error-log))))

(defconst my-test-tty-live--after-init-modes
  '(doom-modeline-mode
    display-time-mode)
  "after-init 後に有効でなければならないグローバルモード。")

(ert-deftest my-test-tty-live-after-init-modes-enabled ()
  :tags '(:tty-live)
  (dolist (mode my-test-tty-live--after-init-modes)
    (should (default-value mode))))

(ert-deftest my-test-tty-live-deferred-modes-enabled ()
  :tags '(:tty-live)
  (dolist (entry my-test-deferred--feature-modes)
    (should (featurep (car entry)))
    (should (default-value (cdr entry)))))

;; GUI 限定宣言(:if (display-graphic-p))が tty で誤って有効化されないことの検査。
;; :defer/:hook 遅延のためバッチでは検出できず、全タイマー発火後の実 tty でのみ
;; ガード削除(例: 40d6d93 での pulsar ガード除去)を観測できる。
;; nerd-icons は doom-modeline-core が無条件 require するため対象外(既知の例外)。
(defconst my-test-tty-live--gui-only-features
  '(pulsar
    spacious-padding
    nerd-icons-completion
    nyan-mode)
  "全タイマー発火後の tty セッションでロードされてはならない feature。")

;; nerd-icons-dired は :hook トリガー型のため、ガードを外しても
;; 対象モードを開くまで feature はロードされず featurep では検出できない。
;; そこで「tty ではフック登録自体が行われない」ことを検査する。
;; pixel-scroll は組み込みで featurep では判定できないため、同じく登録の有無で検査する。
(defconst my-test-tty-live--gui-only-hook-guards
  '((nerd-icons-dired-mode . dired-mode-hook)
    (pixel-scroll-precision-mode . after-init-hook))
  "tty ではフックへ登録されてはならない (関数 . フック変数) の組。")

(ert-deftest my-test-tty-live-gui-only-features-not-loaded ()
  :tags '(:tty-live)
  (dolist (feature my-test-tty-live--gui-only-features)
    (should-not (featurep feature)))
  (dolist (entry my-test-tty-live--gui-only-hook-guards)
    (let ((hook (cdr entry)))
      (should-not (and (boundp hook)
                       (memq (car entry) (symbol-value hook)))))))

;; corfu-popupinfo は子フレームで描画するため、tty 子フレームがある Emacs では
;; tty でも corfu-mode-hook へ登録する。無い Emacs では GUI 限定のままとする。
(ert-deftest my-test-tty-live-corfu-popupinfo-hook ()
  :tags '(:tty-live)
  (should (eq (and (boundp 'corfu-mode-hook)
                   (memq 'corfu-popupinfo-mode corfu-mode-hook)
                   t)
              (and (featurep 'tty-child-frames) t)))
  (when (featurep 'tty-child-frames)
    (with-temp-buffer
      (corfu-mode 1)
      (should (bound-and-true-p corfu-popupinfo-mode)))))

;;;;; [Group] TTY Live - モードライン ;;;;;
;; doom-modeline-icon の :custom (display-graphic-p) は doom-modeline ロード後に
;; 初めて値へ反映されるため、ロード済みの実 tty でのみ検証できる
;; (バッチでは unbound のままで revert しても緑になることを実証済み)。
;; tty でアイコンが有効だと Nerd Font グリフが幅計算を狂わせモードラインが崩れる
;; (560da6d の回帰検出の本体)。
(ert-deftest my-test-tty-live-doom-modeline-icon-disabled ()
  :tags '(:tty-live)
  (should (featurep 'doom-modeline))
  (should-not (default-value 'doom-modeline-icon)))

;; 総行数は doom-modeline の buffer-position セグメントが「行/総行数」で表示する。
;; (旧 total-lines の mode-line-front-space 追記は main レイアウトが参照せず表示されなかった)
(ert-deftest my-test-tty-live-total-line-number-in-mode-line ()
  :tags '(:tty-live)
  (should (default-value 'doom-modeline-total-line-number))
  (with-current-buffer (window-buffer (selected-window))
    (should (string-match-p (regexp-quote (format "/%d" (line-number-at-pos (point-max))))
                            (format-mode-line mode-line-format)))))

(ert-deftest my-test-tty-live-format-mode-line-sanity ()
  :tags '(:tty-live)
  (with-current-buffer (window-buffer (selected-window))
    (let ((formatted-mode-line (format-mode-line mode-line-format)))
      (should (stringp formatted-mode-line))
      (should-not (string-empty-p formatted-mode-line))
      (should (string-match-p (regexp-quote (buffer-name))
                              formatted-mode-line))
      (let ((case-fold-search t))
        (should-not (string-match-p "error" formatted-mode-line))))))

(ert-deftest my-test-tty-live-mode-line-face-background ()
  :tags '(:tty-live)
  (should-not (eq (face-attribute 'mode-line :background nil t)
                  'unspecified)))

;;;;; [Group] TTY Live - 端末初期化 ;;;;;
(ert-deftest my-test-tty-live-display-color-cells ()
  :tags '(:tty-live)
  (should (>= (display-color-cells) 256)))

(ert-deftest my-test-tty-live-terminal-harness-canary ()
  :tags '(:tty-live)
  (let ((terminal-initted (terminal-parameter nil 'terminal-initted)))
    (should (symbolp terminal-initted))
    (should (string-prefix-p "terminal-init-xterm"
                             (symbol-name terminal-initted))))
  (should (equal (lookup-key input-decode-map "\e[1;5A")
                 [C-up]))
  ;; <f9> (折りたたみ) は端末が送る CSI 20~ から復号できること
  (should (equal (lookup-key input-decode-map "\e[20~")
                 [f9])))

;;;;; [Group] TTY Live - 宣言とモード活性化 ;;;;;
(ert-deftest my-test-tty-live-ct-bindings ()
  :tags '(:tty-live)
  (dolist (binding (append my-test-keybindings--bindings
                           my-test-keybindings--ct-extra-bindings))
    (should (eq (lookup-key ggtags-mode-map (kbd (car binding)))
                (cdr binding)))))

;;;;; [Group] TTY Live - タグ検索の入力と結果選択 ;;;;;
(defmacro my-test-tty-live--with-gtags (result-lines &rest body)
  "RESULT-LINES に対応する検索結果を供給し、実ミニバッファで BODY を検査する。
BODY では source-buffer、target-buffer、source-point、requests、
candidate-flags、stages を参照できる。Global と LSP だけを隔離し、
completing-read、Vertico、Consult、xref 履歴は実際の実装を使う。"
  (declare (indent 1) (debug t))
  `(let* ((directory (make-temp-file "my-test-tty-gtags-" t))
          (source-file (expand-file-name "source.txt" directory))
          (target-file (expand-file-name "target.txt" directory))
          (source-text "alpha_symbol\nunchanged source\n")
          (target-text "first match\nsecond match\n")
          (lines ,result-lines)
          (xref--history (cons nil nil))
          (xref-history-storage #'xref-global-history)
          source-buffer target-buffer source-point
          requests candidate-flags stages)
     (unwind-protect
         (save-window-excursion
           (write-region source-text nil source-file nil 'silent)
           (write-region target-text nil target-file nil 'silent)
           (setq source-buffer (find-file-noselect source-file)
                 target-buffer (find-file-noselect target-file))
           (switch-to-buffer source-buffer)
           ;; ggtags のプロジェクト初期化や C モードの Eglot 起動には依存しない。
           (use-local-map ggtags-mode-map)
           (goto-char (point-min))
           (setq source-point (point))
           (let ((minibuffer-setup-hook
                  (cons (lambda ()
                          (push (list (minibuffer-contents-no-properties)
                                      (eq (selected-window)
                                          (active-minibuffer-window))
                                      (and minibuffer-completion-table t))
                                stages))
                        minibuffer-setup-hook)))
             (cl-letf (((symbol-function 'my/gtags--lsp-p) (lambda () nil))
                       ((symbol-function 'my/gtags--candidates)
                        (lambda (flag)
                          (push flag candidate-flags)
                          '("alpha_symbol" "beta_symbol" "gamma_symbol")))
                       ((symbol-function 'my/gtags--run)
                        (lambda (flag input)
                          (push (list flag input) requests)
                          (mapcar
                           (lambda (line)
                             (xref-make (if (= line 1) "first match" "second match")
                                        (xref-make-file-location target-file line 0)))
                           lines))))
               ,@body))
           ;; 早期ジャンプ後に RET や検索語が本文へ入る退行を両バッファで検出する。
           (with-current-buffer source-buffer
             (should (equal (buffer-string) source-text))
             (should-not (buffer-modified-p)))
           (with-current-buffer target-buffer
             (should (equal (buffer-string) target-text))
             (should-not (buffer-modified-p))))
       (dolist (buffer (list source-buffer target-buffer))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory directory t))))

(defun my-test-tty-live--assert-gtags-stages (stages count)
  "STAGES が COUNT 回の補完入力を正しいウィンドウで開いたことを検査する。"
  (should (= (length stages) count))
  (should (equal (caar (last stages)) "alpha_symbol"))
  (dolist (stage stages)
    (should (nth 1 stage))
    (should (nth 2 stage))))

(ert-deftest my-test-tty-live-gtags-single-result-confirm-and-history ()
  :tags '(:tty-live)
  (dolist (binding '(("C-t C-d" . "-d")
                     ("C-t C-u" . "-r")
                     ("C-t C-v" . "-s")))
    (my-test-tty-live--with-gtags '(1)
      ;; 初期シンボルを編集し、TAB で補完してから結果を別の RET で確定する。
      (execute-kbd-macro
       (kbd (concat (car binding) " C-a C-k beta TAB RET C-n C-p RET")))
      (my-test-tty-live--assert-gtags-stages stages 2)
      (should (equal candidate-flags (list (cdr binding))))
      (should (equal requests (list (list (cdr binding) "beta_symbol"))))
      (should (eq (current-buffer) target-buffer))
      (should (= (line-number-at-pos) 1))
      (xref-go-back)
      (should (eq (current-buffer) source-buffer))
      (should (= (point) source-point))
      (xref-go-forward)
      (should (eq (current-buffer) target-buffer))
      (should (= (line-number-at-pos) 1)))))

(ert-deftest my-test-tty-live-gtags-multiple-results-filter-and-select ()
  :tags '(:tty-live)
  (my-test-tty-live--with-gtags '(1 2)
    ;; 検索語は初期値を確定し、Consult 側で結果本文を絞り込む。
    (execute-kbd-macro
     (kbd "C-t C-u RET C-n C-p second TAB RET"))
    (my-test-tty-live--assert-gtags-stages stages 2)
    (should (equal requests '(("-r" "alpha_symbol"))))
    (should (eq (current-buffer) target-buffer))
    (should (= (line-number-at-pos) 2))
    (xref-go-back)
    (should (eq (current-buffer) source-buffer))
    (should (= (point) source-point))
    (xref-go-forward)
    (should (eq (current-buffer) target-buffer))
    (should (= (line-number-at-pos) 2))))

(ert-deftest my-test-tty-live-gtags-lsp-ret-preserves-at-point-symbol ()
  :tags '(:tty-live)
  (my-test-tty-live--with-gtags nil
    (write-region "" nil (expand-file-name "GTAGS" directory) nil 'silent)
    (let ((find-executable (symbol-function 'executable-find))
          lsp-input)
      (cl-letf (((symbol-function 'my/gtags--lsp-p) (lambda () t))
                ((symbol-function 'executable-find)
                 (lambda (program &optional remote)
                   (if (equal program "global")
                       (expand-file-name "global" directory)
                     (funcall find-executable program remote))))
                ((symbol-function 'my/gtags--candidates)
                 (lambda (flag)
                   (push flag candidate-flags)
                   '("alpha_symbol_more")))
                ((symbol-function 'xref-find-definitions)
                 (lambda (symbol)
                   (setq lsp-input symbol)
                   nil)))
        ;; Global に似た名前しかなくても、RET は LSP の元シンボルを確定する。
        (execute-kbd-macro (kbd "C-t C-d RET"))
        (my-test-tty-live--assert-gtags-stages stages 1)
        (should (equal candidate-flags '("-d")))
        (should (equal lsp-input "alpha_symbol"))
        (should-not requests)
        (should (eq (current-buffer) source-buffer))
        (should (= (point) source-point))))))

(ert-deftest my-test-tty-live-gtags-cancel-query ()
  :tags '(:tty-live)
  (my-test-tty-live--with-gtags '(1)
    (condition-case nil
        (execute-kbd-macro (kbd "C-t C-d C-a C-k beta TAB C-g"))
      (quit nil))
    (my-test-tty-live--assert-gtags-stages stages 1)
    (should-not requests)
    (should (eq (current-buffer) source-buffer))
    (should (= (point) source-point))))

(ert-deftest my-test-tty-live-gtags-cancel-results ()
  :tags '(:tty-live)
  (dolist (lines '((1) (1 2)))
    (my-test-tty-live--with-gtags lines
      (condition-case nil
          (execute-kbd-macro (kbd "C-t C-v RET C-n C-p C-g"))
        (quit nil))
      (my-test-tty-live--assert-gtags-stages stages 2)
      (should (equal requests '(("-s" "alpha_symbol"))))
      (should (eq (current-buffer) source-buffer))
      (should (= (point) source-point)))))

;;;;; [Group] TTY Live - LSP 階層ナビゲーションのフォールバック ;;;;;
;; LSP 管理下でない実 tty では、呼び出し元検索が C-t u と同じ Global 経路へ倒れ、
;; 呼び出し先検索は何もせずに失敗する。
(ert-deftest my-test-tty-live-lsp-callers-fallback ()
  :tags '(:tty-live)
  (my-test-tty-live--with-gtags '(1 2)
    (execute-kbd-macro (kbd "C-t C-c RET C-n C-p second TAB RET"))
    (my-test-tty-live--assert-gtags-stages stages 2)
    (should (equal requests '(("-r" "alpha_symbol"))))
    (should (eq (current-buffer) target-buffer))
    (should (= (line-number-at-pos) 2))
    (xref-go-back)
    (should (eq (current-buffer) source-buffer))
    (should (= (point) source-point))))

(ert-deftest my-test-tty-live-lsp-callees-requires-lsp ()
  :tags '(:tty-live)
  (my-test-tty-live--with-gtags nil
    (should-error (execute-kbd-macro (kbd "C-t C-e")) :type 'user-error)
    (should-not stages)
    (should-not requests)
    (should (eq (current-buffer) source-buffer))
    (should (= (point) source-point))))

;; ヘッダ/ソース切替は非 LSP では ff-find-other-file へ倒れ、同じディレクトリの対を開く。
;; C バッファを実際に開くため、C モードのフック群 (ggtags, hideshow など) も実 tty で走る。
(ert-deftest my-test-tty-live-lsp-other-file-fallback ()
  :tags '(:tty-live)
  (let* ((directory (make-temp-file "my-test-tty-other-file-" t))
         (source-file (expand-file-name "pair.c" directory))
         (header-file (expand-file-name "pair.h" directory))
         (xref--history (cons nil nil))
         (xref-history-storage #'xref-global-history)
         source-buffer header-buffer)
    (unwind-protect
        (save-window-excursion
          (write-region "#include \"pair.h\"\nint pair(void) { return 1; }\n"
                        nil source-file nil 'silent)
          (write-region "int pair(void);\n" nil header-file nil 'silent)
          (setq source-buffer (find-file-noselect source-file))
          (switch-to-buffer source-buffer)
          (goto-char (point-max))
          (should (eq (key-binding (kbd "C-t C-o")) 'my/lsp-find-other-file))
          (execute-kbd-macro (kbd "C-t C-o"))
          (setq header-buffer (current-buffer))
          (should (equal (buffer-file-name header-buffer) header-file))
          (xref-go-back)
          (should (eq (current-buffer) source-buffer))
          (should (= (point) (point-max))))
      (dolist (buffer (list source-buffer header-buffer))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory directory t))))

;;;;; [Group] TTY Live - 読解補助 (breadcrumb / outline / inlay hints) ;;;;;
;; C バッファを実 tty で開き、header-line のパス表示と読解補助キーが有効なことを検査する。
;; 文法は一時ルートへ複製されないため通常は cc-mode になる (ts なら outline も有効)。
(ert-deftest my-test-tty-live-cc-reading-aids ()
  :tags '(:tty-live)
  (let* ((directory (make-temp-file "my-test-tty-reading-" t))
         (file (expand-file-name "sample.c" directory))
         buffer)
    (unwind-protect
        (save-window-excursion
          (write-region "int sample(void) {\n    return 1;\n}\n" nil file nil 'silent)
          (setq buffer (find-file-noselect file))
          (switch-to-buffer buffer)
          (should (derived-mode-p 'c-mode))
          ;; breadcrumb: header-line にファイル名を含むパスが出る
          (should breadcrumb-local-mode)
          (should (member '(:eval (breadcrumb--header-line)) header-line-format))
          (let ((line (format-mode-line header-line-format)))
            (should (stringp line))
            (should (string-match-p "sample\\.c" line))
            (let ((case-fold-search t))
              (should-not (string-match-p "error" line))))
          ;; outline は treesit 連動の見出しが取れる ts モードだけ
          (if (my/treesit-cc-ready-p 'c)
              (should outline-minor-mode)
            (should-not (bound-and-true-p outline-minor-mode)))
          (should (eq (key-binding [f9]) 'my/outline-cycle-dwim))
          (should (eq (key-binding (kbd "C-t l")) 'my/eglot-inlay-hints-toggle))
          ;; eglot 未管理 (CDB なし) では inlay hints のトグルは失敗する
          (should-error (my/eglot-inlay-hints-toggle) :type 'user-error))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (bound-and-true-p breadcrumb--idle-timer)
            (cancel-timer breadcrumb--idle-timer))
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest my-test-tty-live-corfu-terminal-enabled ()
  :tags '(:tty-live)
  (if (featurep 'tty-child-frames)
      (should-not (bound-and-true-p corfu-terminal-mode))
    (should (default-value 'corfu-terminal-mode))))

(defun my-test-tty-live--xclip-expected-p ()
  "現在の端末環境で xclip-mode が有効になるべき場合は non-nil を返す。"
  (and (not (display-graphic-p))
       (getenv "DISPLAY")
       (executable-find "xclip")
       t))

(ert-deftest my-test-tty-live-xclip-normalized ()
  :tags '(:tty-live)
  (should (eq (and (bound-and-true-p xclip-mode) t)
              (and (not (display-graphic-p))
                   (getenv "DISPLAY")
                   (executable-find "xclip")
                   t))))

(ert-deftest my-test-tty-live-gzip-command-line ()
  :tags '(:tty-live)
  (should (bound-and-true-p my-test-tty-after-init-handlers))
  (let ((buffer (get-file-buffer (my-set-emacs "tty-fixture.txt.gz"))))
    (should (buffer-live-p buffer))
    (with-current-buffer buffer
      (should (equal (buffer-string) "tty gzip fixture\n")))))

(ert-deftest my-test-tty-live-capf-commit ()
  :tags '(:tty-live)
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (corfu-mode 1)
      (setq-local completion-at-point-functions
                  (list (lambda () (list (point-min) (point-max) '("alpha" "alpine")))))
      (insert "al")
      (unwind-protect
          (progn
            (completion-at-point)
            (corfu--exhibit)
            (should (member "alpha" corfu--candidates))
            (if (featurep 'tty-child-frames)
                (progn
                  (should (frame-live-p corfu--frame))
                  (should (frame-visible-p corfu--frame))
                  (should (eq (frame-parent corfu--frame) (selected-frame))))
              (should (bound-and-true-p corfu-terminal-mode)))
            (corfu-next (- (cl-position "alpha" corfu--candidates :test #'equal) corfu--index))
            (corfu-insert)
            (should (equal (buffer-string) "alpha")))
        (corfu-quit)))))

(ert-deftest my-test-tty-live-persistent-undo ()
  :tags '(:tty-live)
  ;; fixture の HOME 自体が /tmp にあるため、この検査中だけ一時パス除外を外す。
  (let* ((undo-fu-session-ignore-temp-files nil)
         (buffer (find-file-noselect (my-set-emacs "tty-persistent.txt"))))
    (unwind-protect
        (with-current-buffer buffer
          (should (bound-and-true-p undo-fu-session-mode))
          (if (equal (getenv "MY_TTY_TEST_PHASE") "write")
              (progn
                (should (equal (buffer-string) "before\n"))
                (setq buffer-undo-list nil)
                (goto-char (point-max))
                (insert "after\n")
                (undo-boundary)
                (save-buffer))
            (should (equal (buffer-string) "before\nafter\n"))
            (should (consp buffer-undo-list))
            (undo-only 1)
            (should (equal (buffer-string) "before\n"))))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(ert-deftest my-test-tty-live-input-method-transition ()
  :tags '(:tty-live)
  ;; 実 Mozc との通信は手動統合検証。ここでは標準コマンドからの状態遷移を守る。
  (let ((input-method-alist (copy-tree input-method-alist)))
    (register-input-method "my-test-japanese" "Japanese"
                           (lambda (&rest _)
                             (setq input-method-function #'list
                                   deactivate-current-input-method-function
                                   (lambda () (setq input-method-function nil)))) "試" "固定fixture")
    (with-temp-buffer
      (let ((default-input-method "my-test-japanese"))
        (call-interactively #'toggle-input-method)
        (should (equal current-input-method "my-test-japanese"))
        (should (equal (funcall input-method-function ?あ) '(?あ)))
        (call-interactively #'toggle-input-method)
        (should-not current-input-method)))))

;;;;; [Group] TTY Live - window-setup ランナー ;;;;;
(defun my-test-tty-live--deferred-ready-p ()
  "遅延ロード対象と条件付き xclip が有効なら non-nil を返す。"
  (let ((ready t))
    (dolist (entry my-test-deferred--feature-modes)
      (unless (and (featurep (car entry))
                   (boundp (cdr entry))
                   (default-value (cdr entry)))
        (setq ready nil)))
    (and ready
         (or (not (my-test-tty-live--xclip-expected-p))
             (and (featurep 'xclip)
                  (bound-and-true-p xclip-mode))))))

(defun my-test-tty-live--run (&optional from-idle-timer)
  "遅延ロードを待って tty-live テストを実行し、Emacs を終了する。"
  (if (not from-idle-timer)
      ;; window-setup-hook のスタック中は :defer の idle timer が進まないため、
      ;; 実起動が最初に idle になった時点へ同じランナーを引き渡す。
      (run-with-idle-timer 0 nil #'my-test-tty-live--run t)
    (condition-case err
        (let ((attempts 0)
              stats
              failures)
          (while (and (< attempts 100)
                      (not (my-test-tty-live--deferred-ready-p)))
            (setq attempts (1+ attempts))
            (sit-for 0.1))

          (setq stats (ert-run-tests-batch '(tag :tty-live)))

          (let ((expected-total
                 (length (ert-select-tests '(tag :tty-live) t)))
                (actual-total (ert-stats-total stats)))
            (unless (and (> actual-total 0)
                         (= actual-total expected-total))
              (push (format "ERT 件数が不正です: actual=%d expected=%d"
                            actual-total expected-total)
                    failures)))

          (unless (= (ert-stats-skipped stats) 0)
            (push (format "ERT に skip があります: %d"
                          (ert-stats-skipped stats))
                  failures))

          (unless (= (ert-stats-completed-unexpected stats) 0)
            (push (format "ERT に予期しない結果があります: %d"
                          (ert-stats-completed-unexpected stats))
                  failures))

          ;; 最終描画と待機を先に済ませ、この区間で発生した警告も
          ;; 直後の最終検査で取りこぼさず fail-closed にする。
          (redisplay t)
          (sit-for 0.5)

          (let ((warnings (my-test-startup-check-warnings)))
            (when warnings
              (dolist (warning warnings)
                (princ
                 (format "tty-live 起動時警告: type=%S level=%S message=%s\n"
                         (nth 0 warning)
                         (nth 2 warning)
                         (format "%s" (nth 1 warning)))
                 #'external-debugging-output))
              (push (format "未許可の起動時警告があります: %d"
                            (length warnings))
                    failures)))

          (setq failures (nreverse failures))
          (dolist (failure failures)
            (princ (format "tty-live 失敗: %s\n" failure)
                   #'external-debugging-output))
          (kill-emacs (if failures 1 0)))
      (error
       (princ (format "tty-live ランナーエラー: %S\n" err)
              #'external-debugging-output)
       (kill-emacs 1)))))

(add-hook 'window-setup-hook #'my-test-tty-live--run 90)

(provide 'my-test-tty-live)
;;; my-test-tty-live.el ends here
