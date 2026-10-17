;;; my-runtime.el --- 実行時データの保存先の移行 -*- lexical-binding: t; -*-
;;; Commentary:
;; early-init.el が、履歴やパッケージを読み込む前に旧配置を移行する。
;; 移行済みの項目は何もしない。同名ファイルとリンクは自動で解決しない。
;; 途中で止まっても移動済みの内容は残り、衝突を解消して再実行できる。

;;; Code:

(defun my/runtime--move (source destination)
  "SOURCE を DESTINATION へ移し、既存の内容を上書きしない。
両方が通常のディレクトリなら、中身を再帰的に移す。
同名ファイルやシンボリックリンクがあれば、双方を残してエラーにする。"
  (when (or (file-exists-p source) (file-symlink-p source))
    (cond
     ((or (file-symlink-p source) (file-symlink-p destination))
      (error "保存先の移行: リンクを手動で確認してください: %s → %s"
             source destination))
     ((not (file-exists-p destination))
      (make-directory (file-name-directory destination) t)
      (rename-file source destination))
     ((and (file-directory-p source) (file-directory-p destination))
      (dolist (name (directory-files source nil directory-files-no-dot-files-regexp))
        (my/runtime--move (expand-file-name name source)
                          (expand-file-name name destination)))
      (delete-directory source))
     (t
      (error "保存先の移行: 同名ファイルを上書きできません: %s → %s"
             source destination)))))

(defun my/runtime-migrate-layout (root)
  "ROOT 配下の旧保存先を var の用途別ディレクトリへ移す。
旧 Emacs を終了してから新しい設定を起動する。ファイルの内容は変更しない。
ルート直下の eln-cache は現用キャッシュと混ぜず、cache 内へ退避する。"
  ;; 親を移してから、その中で用途の異なる項目を移す。
  (dolist (entry '(("var/hist" . "var/state")
                   ("var/package" . "var/packages")
                   ("var/database" . "var/state/database")
                   ("var/state/irony" . "var/packages/irony")
                   ("var/packages/eln-cache" . "var/cache/eln-cache")
                   ("projects.eld" . "var/state/projects.eld")
                   ("projects" . "var/state/projects")
                   ("transient" . "var/state/transient")
                   ("url" . "var/state/url")
                   ("request" . "var/state/request")
                   ("copilot-chat" . "var/state/copilot-chat")
                   (".cache/copilot" . "var/packages/copilot")
                   ("eln-cache" . "var/cache/legacy-eln-cache")
                   (".bench" . "var/cache/bench")))
    ;; .cache 自体がリンクなら、その参照先には手を加えない。
    (unless (and (equal (car entry) ".cache/copilot")
                 (file-symlink-p (expand-file-name ".cache" root)))
      (my/runtime--move (expand-file-name (car entry) root)
                        (expand-file-name (cdr entry) root))))
  ;; 不明なキャッシュや、利用者が張ったリンクは残す。
  (let ((old-cache (expand-file-name ".cache" root)))
    (when (and (file-directory-p old-cache)
               (not (file-symlink-p old-cache))
               (directory-empty-p old-cache))
      (delete-directory old-cache))))

(provide 'my-runtime)
;;; my-runtime.el ends here
