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
  :custom
  ;; 全角の説明も実表示幅で列揃えする。:custom は遅延ロード前に適用され、
  ;; Magit を開かず Copilot Chat を使う場合にも同じ Transient 設定が届く。
  (transient-align-variable-pitch t)
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

;;; 日常の下位メニュー - 固定説明だけを変更
;; 動的な対象名・警告・引数値は upstream の表示を保つ。
(with-eval-after-load 'magit-diff
  (dolist (entry
           `(((0) . "対象の限定")
             ((1) . "前後の表示")
             ((2) . "表示の調整")
             ("--" . "ファイルを限定")
             ("-b" . "空白量を無視")
             ("-w" . "全空白を無視")
             ("-D" . "削除前を省略")
             ("-U" . "前後の行数")
             ("-W" . "関数全体を表示")
             (magit-diff:--ignore-submodules . "サブモジュールを無視")
             (magit-diff:--diff-algorithm . "差分の計算法")
             (magit-diff:--diff-merges . "マージの差分")
             (magit-diff:-M . "名前変更を検出")
             (magit-diff:-C . "コピーを検出")
             (magit-diff:-R . "比較方向を反転")
             (magit-diff:--color-moved . "移動行を着色")
             (magit-diff:--color-moved-ws . "着色時の空白")
             (magit-diff:--no-ext-diff . "外部差分を無効化")
             (magit-diff:--stat . "集計を表示")
             (magit-diff:--show-signature . "署名を表示")))
    (transient-suffix-put 'magit-diff-infix-arguments (car entry) :description (cdr entry)))
  (dolist (entry
           `(((1) . "差分の操作")
             ("d" . ,(my/key-guide-description 'magit-diff-dwim))
             ("r" . ,(my/key-guide-description 'magit-diff-range))
             ("p" . ,(my/key-guide-description 'magit-diff-paths))
             ("u" . ,(my/key-guide-description 'magit-diff-unstaged))
             ("s" . ,(my/key-guide-description 'magit-diff-staged))
             ("w" . ,(my/key-guide-description 'magit-diff-working-tree))
             ("c" . ,(my/key-guide-description 'magit-show-commit))
             ("t" . ,(my/key-guide-description 'magit-stash-show))))
    (transient-suffix-put 'magit-diff (car entry) :description (cdr entry)))
  (dolist (entry
           `(((1 0) . "再表示")
             ((1 1) . "切り替え")
             ((1 2) . "操作")
             ("g" . "再表示")
             ("s" . "既定値に設定")
             ("w" . "既定値を保存")
             ("t" . ,(my/key-guide-description 'magit-diff-toggle-refine-hunk))
             ("F" . ,(my/key-guide-description 'magit-diff-toggle-file-filter))
             ("b" . ,(my/key-guide-description 'magit-toggle-buffer-lock))
             ("r" . ,(my/key-guide-description 'magit-diff-switch-range-type))
             ("f" . ,(my/key-guide-description 'magit-diff-flip-revs))))
    (transient-suffix-put 'magit-diff-refresh (car entry) :description (cdr entry))))

(with-eval-after-load 'magit-commit
  (dolist (entry
           `(((0) . "引数")
             ((1 0) . "作成")
             ((1 1) . "直前の修正")
             ((1 2) . "過去の修正")
             ((1 3) . "即時統合")
             ((1 4) . "分散")
             ("-a" . "追跡中の変更を全追加")
             ("-e" . "空コミットを許可")
             ("-v" . "差分を表示")
             ("-n" . "フックを無効化")
             ("-R" . "作成者と日時を更新")
             (magit:--author . "作成者を指定")
             (magit-commit:--date . "作成日時を指定")
             (magit:--gpg-sign . "GPG 署名")
             (magit:--signoff . "署名行を追加")
             (magit-commit:--reuse-message . "メッセージを再利用")
             ("c" . ,(my/key-guide-description 'magit-commit-create))
             ("e" . ,(my/key-guide-description 'magit-commit-extend))
             ("a" . ,(my/key-guide-description 'magit-commit-amend))
             ("w" . ,(my/key-guide-description 'magit-commit-reword))
             ("d" . ,(my/key-guide-description 'magit-commit-reshelve))
             ("f" . ,(my/key-guide-description 'magit-commit-fixup))
             ("s" . ,(my/key-guide-description 'magit-commit-squash))
             ("A" . ,(my/key-guide-description 'magit-commit-alter))
             ("n" . ,(my/key-guide-description 'magit-commit-augment))
             ("W" . ,(my/key-guide-description 'magit-commit-revise))
             ("F" . ,(my/key-guide-description 'magit-commit-instant-fixup))
             ("S" . ,(my/key-guide-description 'magit-commit-instant-squash))
             ("R" . ,(my/key-guide-description 'magit-rebase-reword-commit))
             ("x" . ,(my/key-guide-description 'magit-commit-autofixup))
             ("X" . ,(my/key-guide-description 'magit-commit-absorb-modules))))
    (transient-suffix-put 'magit-commit (car entry) :description (cdr entry))))

(with-eval-after-load 'magit-branch
  (dolist (entry
           `(((1) . "リポジトリの既定値")
             ((2) . "引数")
             ((3 0) . "切替")
             ((3 2) . "作成")
             ((3 3) . "操作")
             ("B" . "既定ブランチ更新")
             ("-r" . "サブモジュールも切替")
             ("b" . ,(my/key-guide-description 'magit-checkout))
             ("l" . ,(my/key-guide-description 'magit-branch-checkout))
             ("o" . ,(my/key-guide-description 'magit-branch-orphan))
             ("c" . ,(my/key-guide-description 'magit-branch-and-checkout))
             ("s" . ,(my/key-guide-description 'magit-branch-spinoff))
             ("w" . ,(my/key-guide-description 'magit-worktree-checkout))
             ("n" . ,(my/key-guide-description 'magit-branch-create))
             ("S" . ,(my/key-guide-description 'magit-branch-spinout))
             ("W" . ,(my/key-guide-description 'magit-worktree-branch))
             ("C" . ,(my/key-guide-description 'magit-branch-configure))
             ("m" . ,(my/key-guide-description 'magit-branch-rename))
             ("x" . ,(my/key-guide-description 'magit-branch-reset))
             ("k" . ,(my/key-guide-description 'magit-branch-delete))
             ("h" . ,(my/key-guide-description 'magit-branch-shelve))
             ("H" . ,(my/key-guide-description 'magit-branch-unshelve))))
    (transient-suffix-put 'magit-branch (car entry) :description (cdr entry)))
  (dolist (entry
           `(((1) . "リポジトリの既定値")
             ((2) . "作成時の設定")
             ("B" . "既定ブランチ更新")))
    (transient-suffix-put 'magit-branch-configure (car entry) :description (cdr entry))))

(with-eval-after-load 'magit-push
  (dolist (entry
           `(((0) . "引数")
             ((2) . "送信")
             ((3) . "設定")
             ("-f" . "変更がなければ強制")
             ("-F" . "強制送信")
             ("-h" . "フックを無効化")
             ("-n" . "試行のみ")
             ("-u" . "追跡先に設定")
             ("-T" . "全タグも送信")
             ("-t" . "関連する注釈タグも送信")
             ("e" . ,(my/key-guide-description 'magit-push-current))
             ("o" . ,(my/key-guide-description 'magit-push-other))
             ("r" . ,(my/key-guide-description 'magit-push-refspecs))
             ("m" . ,(my/key-guide-description 'magit-push-matching))
             ("T" . ,(my/key-guide-description 'magit-push-tag))
             ("t" . ,(my/key-guide-description 'magit-push-tags))
             ("n" . ,(my/key-guide-description 'magit-push-notes-ref))
             ("C" . "変数を設定")))
    (transient-suffix-put 'magit-push (car entry) :description (cdr entry))))

(with-eval-after-load 'magit-pull
  (dolist (entry
           `(((2) . "情報の取得元")
             ((3) . "情報の取得")
             ((4) . "設定")
             ("-f" . "早送りのみ")
             ("-r" . "ローカル履歴を再配置")
             ("-A" . "自動退避")
             ("-F" . "強制取得")
             ("e" . ,(my/key-guide-description 'magit-pull-branch))
             ("f" . "全リモート")
             ("F" . "全取得・不要参照削除")
             ("o" . "別ブランチ")
             ("s" . "参照を指定")
             ("m" . "サブモジュール")
             ("C" . "変数を設定")))
    (transient-suffix-put 'magit-pull (car entry) :description (cdr entry))))

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
      [("D" "変更を構文比較" difftastic-magit-diff)
       ("S" "履歴を構文比較" difftastic-magit-show)])))

;;; Dsvn - SVN 管理ツール
(use-package dsvn
  :straight t
  :defer t
  :commands (svn-status svn-update))

(provide '33-vcs)
;;; 33-vcs.el ends here
