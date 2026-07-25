;;; my-test-tty-live.el --- 実 pty での tty 起動回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; shim と `--init-directory` により、実起動と同一のライフサイクルで検証する。
;; shim は straight override、native-comp パリティ、警告レコーダー、load のみに
;; 限定し、実 early-init 本体は無改変のまま load する。
;; 検査対象のフック面と宣言は依存パッケージ更新で変わり得る可変集合である。
;; 失敗時は設定の不具合と断定する前に、まず依存側の変化を確認して切り分ける。
;; 回帰点を絞るため、アサーションをこの最小集合より増やさない方針とする。

;;; Code:

(require 'ert)
(require 'my-test-keybindings)
(require 'my-test-deferred)
(require 'my-test-startup)

(when noninteractive
  (error "my-test-tty-live は実 pty 専用"))

;;;;; [Group] TTY Live - 起動ライフサイクル ;;;;;
(ert-deftest my-test-tty-live-init-loader-errors-empty ()
  :tags '(:tty-live)
  (should (string-empty-p (init-loader-error-log))))

(defconst my-test-tty-live--after-init-modes
  '(doom-modeline-mode
    display-time-mode
    global-total-lines-mode)
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

;; nerd-icons-dired / corfu-popupinfo は :hook トリガー型のため、ガードを外しても
;; 対象モードを開くまで feature はロードされず featurep では検出できない。
;; そこで「tty ではフック登録自体が行われない」ことを検査する。
(defconst my-test-tty-live--gui-only-hook-guards
  '((nerd-icons-dired-mode . dired-mode-hook)
    (corfu-popupinfo-mode . corfu-mode-hook))
  "tty ではフックへ登録されてはならない (関数 . フック変数) の組。")

(ert-deftest my-test-tty-live-gui-only-features-not-loaded ()
  :tags '(:tty-live)
  (dolist (feature my-test-tty-live--gui-only-features)
    (should-not (featurep feature)))
  (dolist (entry my-test-tty-live--gui-only-hook-guards)
    (let ((hook (cdr entry)))
      (should-not (and (boundp hook)
                       (memq (car entry) (symbol-value hook)))))))

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

(defconst my-test-tty-live--total-lines-mode-line-entry
  '(:eval (when (bound-and-true-p total-lines)
            (format " (%d)" (- total-lines 1))))
  "total-lines が追加する mode-line-front-space の要素。")

(ert-deftest my-test-tty-live-total-lines-mode-line-entry ()
  :tags '(:tty-live)
  (should (member my-test-tty-live--total-lines-mode-line-entry
                  mode-line-front-space)))

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
                 [C-up])))

;;;;; [Group] TTY Live - 宣言とモード活性化 ;;;;;
(ert-deftest my-test-tty-live-ct-bindings ()
  :tags '(:tty-live)
  (dolist (binding my-test-keybindings--bindings)
    (should (eq (lookup-key ggtags-mode-map (kbd (car binding)))
                (cdr binding)))))

(ert-deftest my-test-tty-live-corfu-terminal-enabled ()
  :tags '(:tty-live)
  (should (default-value 'corfu-terminal-mode)))

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
