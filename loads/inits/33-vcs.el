;;; 33-vcs.el --- バージョン管理の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Git, SVN 等のバージョン管理ツール関連パッケージの設定

;;; Code:

;;;;; [Group] Version-control - バージョン管理関連 ;;;;;
;;; Magit
(use-package magit
  :straight t
  :ensure t
  :bind (("C-x G" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (defun mu-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  (bind-key "q" #'mu-magit-kill-buffers magit-status-mode-map)
  )

;;; Diff-hl - 変更点を表示 (git-gutter の置き換え)
(use-package diff-hl
  :straight t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :init
  ;; global-diff-hl-mode は autoload 済み → タイマーで遅延ロード
  (run-with-idle-timer 1 nil (lambda ()
    (global-diff-hl-mode +1)
    (global-diff-hl-show-hunk-mouse-mode +1)
    (diff-hl-margin-mode +1)))
  )

;;; Difftastic
(use-package difftastic
  :straight t
  :after magit
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)]))
  )

;;; with-editor - Emacs から Git コミットメッセージを編集
(use-package with-editor
  :straight t
  :defer t)

;;; Dsvn - SVN 管理ツール
(use-package dsvn
  :straight t
  :defer t
  :commands (svn-status svn-update)
  )

(provide '33-vcs)
;;; 33-vcs.el ends here
