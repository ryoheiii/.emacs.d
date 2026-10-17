;;; 33-vcs.el --- バージョン管理の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Git, SVN 等のバージョン管理ツール関連パッケージの設定

;;; Code:

;; 遅延ロード先とプラットフォーム固有定義をコンパイラへ伝える。
(declare-function magit-restore-window-configuration "magit-mode")
(declare-function magit-mode-get-buffers "magit-mode")

;;; Magit - Git の状態確認と操作
(use-package magit
  :straight t
  :bind (("C-x G" . magit-status)
         ("C-x M-g" . magit-dispatch)
         :map magit-status-mode-map
         ("q" . mu-magit-kill-buffers))
  ;; :bind から参照する関数は :preface で定義する (:init だと多重定義警告が出る)
  :preface
  (defun mu-magit-kill-buffers ()
    "ウィンドウ構成を復元し、現在のリポジトリの Magit バッファをすべて閉じる。"
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  :config
  ;; 公開 API で入口の表示だけを変更する。端末の三列表示に収まる短い説明。
  (dolist (entry '((magit-cherry-pick . "選んで適用")
                   (magit-branch . "ブランチ")
                   (magit-bisect . "原因の二分探索")
                   (magit-commit . "コミット")
                   (magit-clone . "複製")
                   (magit-diff . "差分")
                   (magit-diff-refresh . "差分の表示設定")
                   (magit-ediff-dwim . "対象の差分比較")
                   (magit-ediff . "差分を並べる")
                   (magit-fetch . "情報を取得")
                   (magit-pull . "取得して統合")
                   (magit-info . "マニュアル")
                   (magit-describe-section . "項目の情報")
                   (magit-gitignore . "管理から除外")
                   (magit-init . "新規作成")
                   (magit-status-jump . "項目へ移動")
                   (magit-status-quick . "変更状況")
                   (magit-display-repository-buffer . "バッファ表示")
                   (magit-log . "変更履歴")
                   (magit-log-refresh . "履歴の表示設定")
                   (magit-merge . "マージ")
                   (magit-remote . "接続先")
                   (magit-submodule . "サブモジュール")
                   (magit-subtree . "サブツリー")
                   (magit-push . "変更を送信")
                   (magit-git-command . "コマンド入力")
                   (magit-rebase . "履歴の並べ直し")
                   (magit-tag . "タグ")
                   (magit-notes . "注記")
                   (magit-revert . "打ち消す変更")
                   (magit-am . "パッチ適用")
                   (magit-patch . "パッチ作成")
                   (magit-reset . "過去の状態へ")
                   (magit-show-refs . "ブランチ一覧")
                   (magit-cherry . "未適用の履歴")
                   (magit-stash . "一時退避")
                   (magit-worktree . "作業ツリー")
                   (magit-run . "外部コマンド")
                   (magit-apply . "差分を適用")
                   (magit-reverse . "差分を逆適用")
                   (magit-discard . "変更を破棄")
                   (magit-stage . "確定対象へ追加")
                   (magit-unstage . "確定対象を解除")
                   (magit-stage-modified . "追跡中を全追加")
                   (magit-unstage-all . "全対象を解除")
                   (magit-refresh . "表示を更新")
                   (magit-mode-bury-buffer . "画面を隠す")
                   (magit-section-toggle . "項目の開閉")
                   (magit-visit-thing . "対象を開く")
                   (describe-mode . "全キーの説明")))
    (transient-suffix-put 'magit-dispatch (car entry) :description (cdr entry)))
  (transient-suffix-put 'magit-dispatch '(0) :description "Git の操作")
  (transient-suffix-put 'magit-dispatch '(1) :description "変更の適用")
  (transient-suffix-put 'magit-dispatch '(2) :description "基本操作"))

;;; Diff-hl - 変更点を表示 (git-gutter の置き換え)
(use-package diff-hl
  :straight t
  :defer 1
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :config
  (global-diff-hl-mode +1)
  (global-diff-hl-show-hunk-mouse-mode +1)
  (diff-hl-margin-mode +1))

;;; Difftastic - 構文を考慮した差分表示を Magit から呼ぶ
(use-package difftastic
  :straight t
  :bind (:map magit-blame-read-only-mode-map
              :package magit-blame
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :init
  (with-eval-after-load 'magit-diff
    (transient-append-suffix 'magit-diff '(-1 -1)
      [("D" "変更を構文差分で表示" difftastic-magit-diff)
       ("S" "コミットを構文差分で表示" difftastic-magit-show)])))

;;; Dsvn - SVN 管理ツール
(use-package dsvn
  :straight t
  :defer t
  :commands (svn-status svn-update))

(provide '33-vcs)
;;; 33-vcs.el ends here
