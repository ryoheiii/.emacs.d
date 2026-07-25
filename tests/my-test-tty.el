;;; my-test-tty.el --- tty ロード条件の回帰テスト  -*- lexical-binding: t; -*-
;;; Commentary:
;; バッチ起動時の非 GUI 分岐と tty 向けロード条件を検証する。
;; GUI 限定 feature の未ロード検査は、after-init 未発火の純ロード状態に限定する。
;; nerd-icons は after-init 発火後に doom-modeline-core から無条件に require されるため、
;; この検査を実 tty（将来の :tty-live）へ転用してはならない。
;; pixel-scroll は組み込みで他経路からロードされ得るため、検査対象外とする。

;;; Code:

(require 'ert)

;;;;; [Group] TTY - 端末向け Corfu ;;;;;
(ert-deftest my-test-tty-corfu-terminal-enabled ()
  :tags '(:tty)
  (should (featurep 'corfu-terminal))
  (should (default-value 'corfu-terminal-mode)))

;;;;; [Group] TTY - GUI 限定 feature ;;;;;
(defconst my-test-tty--gui-only-features
  '(pulsar
    spacious-padding
    nerd-icons
    nerd-icons-completion
    nerd-icons-dired
    nyan-mode
    corfu-popupinfo)
  "after-init 未発火の非 GUI 起動でロードされてはならない feature。")

(ert-deftest my-test-tty-gui-only-features-not-loaded ()
  :tags '(:tty)
  (dolist (feature my-test-tty--gui-only-features)
    (should-not (featurep feature))))

;;;;; [Group] TTY - モードラインのアイコン ;;;;;
(ert-deftest my-test-tty-doom-modeline-icon-disabled ()
  :tags '(:tty)
  (should-not (bound-and-true-p doom-modeline-icon)))

;;;;; [Group] TTY - xclip の遅延ロード ;;;;;
(ert-deftest my-test-tty-xclip-deferred ()
  :tags '(:tty)
  ;; 条件が成立する環境でも :defer 0.5 の idle timer 前は未ロードが正しい。
  ;; 条件一致だけを根拠に、ロード済みであることを期待してはならない。
  (should-not (featurep 'xclip)))

(provide 'my-test-tty)
;;; my-test-tty.el ends here
