;;; my-node-path.el --- Node.js の実行パス検出 -*- lexical-binding: t; -*-
;;; Commentary:
;; 35-copilot.el から呼ぶ Node.js の検出処理。
;; 呼び出し元との互換性のため、既存の my/copilot- 関数名を維持する。
;; PATH の更新は my/copilot-setup-node-path を呼んだときだけ行う。

;;; Code:

(require 'seq)
(require 'subr-x)

(defun my/copilot--prepend-to-path (dir)
  "DIR を `exec-path' と環境変数 PATH の先頭に移動する.
既に存在する場合は一度除去してから先頭に再挿入し、優先順を保証する."
  (when (and dir (file-directory-p dir))
    ;; 既存エントリを除去してから先頭に追加
    (setq exec-path (delete dir exec-path))
    (push dir exec-path)
    (let* ((current (or (getenv "PATH") ""))
           (entries (split-string current path-separator t))
           (cleaned (delete dir entries)))
      (setenv "PATH" (string-join (cons dir cleaned) path-separator)))))

(defun my/copilot--resolve-nvm-alias (alias-value nvm-dir &optional depth)
  "ALIAS-VALUE を再帰的に解決し、バージョン文字列を返す.
NVM-DIR は nvm のルート。DEPTH はループ防止用（最大 10）.
数値バージョン（例: \"22\", \"v22.14.0\"）はそのまま返す。
名前エイリアス（例: \"lts/*\", \"node\"）は NVM-DIR/alias/ から再帰解決する。"
  (let ((depth (or depth 0)))
    (when (< depth 10)
      (cond
       ;; 数値バージョン → そのまま返す
       ((string-match-p "\\`v?[0-9]+\\(?:\\.[0-9]+\\)*\\'" alias-value)
        alias-value)
       ;; 名前エイリアス → NVM-DIR/alias/<name> を読んで再帰
       ((not (string-empty-p alias-value))
        (let ((alias-file (expand-file-name (concat "alias/" alias-value) nvm-dir)))
          (when (file-exists-p alias-file)
            (let ((next (string-trim
                         (with-temp-buffer
                           (insert-file-contents alias-file)
                           (buffer-string)))))
              (unless (string-empty-p next)
                (my/copilot--resolve-nvm-alias next nvm-dir (1+ depth)))))))))))

(defun my/copilot-setup-node-path ()
  "Node.js のパスを検出し PATH 先頭に追加する.
優先順: fnm > nvm > emacs-setup.sh --setup-node のオフライン導入先。
低優先度から処理し、最後に追加したものが先頭に来る.
バージョンマネージャを使っている環境ではそちらの選択を優先し、
スクリプトで入れた実体はフォールバックとして扱う."
  ;; 1. emacs-setup.sh --setup-node が作るシンボリックリンク (最低優先度)
  (my/copilot--prepend-to-path (expand-file-name ".local/node/bin" "~"))
  ;; 2. nvm（フォールバック: default エイリアスから解決）
  (let* ((nvm-dir (or (getenv "NVM_DIR") (expand-file-name ".nvm" "~")))
         (nvm-alias-file (expand-file-name "alias/default" nvm-dir)))
    (when (file-exists-p nvm-alias-file)
      (let* ((alias-value (string-trim
                           (with-temp-buffer
                             (insert-file-contents nvm-alias-file)
                             (buffer-string))))
             ;; 再帰的にエイリアスを解決（"lts/*" → "lts/krypton" → "v24.13.0" 等）
             (resolved (my/copilot--resolve-nvm-alias alias-value nvm-dir)))
        (when resolved
          (let* ((version (if (string-prefix-p "v" resolved)
                              resolved
                            (concat "v" resolved)))
                 ;; 部分バージョン（"v22"）→ ディレクトリ glob で最新を取得
                 (node-dirs
                  (seq-filter
                   (lambda (dir)
                     (let ((candidate (file-name-nondirectory
                                       (directory-file-name (file-name-directory dir)))))
                       (and (string-match-p "\\`v[0-9]+\\.[0-9]+\\.[0-9]+\\'" candidate)
                            (or (equal candidate version)
                                (string-prefix-p (concat version ".") candidate)))))
                   (file-expand-wildcards
                    (expand-file-name (concat "versions/node/" version "*/bin") nvm-dir))))
                 ;; セマンティックバージョン比較（string< だと v22.9 > v22.14 になる）
                 ;; version< は "v" 付きだと Invalid version syntax になるため除去
                 (node-bin (car (last (sort node-dirs
                                            (lambda (a b)
                                              (let ((va (replace-regexp-in-string "\\`v" ""
                                                         (file-name-nondirectory
                                                          (directory-file-name (file-name-directory a)))))
                                                    (vb (replace-regexp-in-string "\\`v" ""
                                                         (file-name-nondirectory
                                                          (directory-file-name (file-name-directory b))))))
                                                (version< va vb))))))))
            (when node-bin
              (my/copilot--prepend-to-path node-bin)))))))
  ;; 3. fnm（最優先: 最後に prepend → PATH の先頭に来る）
  (my/copilot--prepend-to-path
   (expand-file-name ".local/share/fnm/aliases/default/bin" "~")))

(provide 'my-node-path)
;;; my-node-path.el ends here
