;;; Code:
;;; ロードパス
;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))
;; load-pathに追加するディレクトリ
(add-to-load-path "loads/elisp/" "loads/my-functions/")

;;; Package Manegement
(require 'package)
;; package.elでelispを入れるdirectoryの設定
(setq package-user-dir "~/.emacs.d/loads/elisp/elpa/")
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;; 起動時に自動でインストール
(require 'cl)
(defvar installing-package-list
  '(
    init-loader
    auto-complete
    fuzzy ; auto-completeの曖昧補完
    migemo ; ローマ字で日本語検索するツール, require cmigemo
    multiple-cursors
    smartrep
    expand-region ;; C-sで検索中にC-wで語句選択可能に
    flycheck
    flycheck-pos-tip ; for flycheck
    exec-path-from-shell
    highlight-symbol
    auto-highlight-symbol
    recentf-ext
    smooth-scroll
    rainbow-delimiters
    color-theme-modern

    ;; translate ;; C-c C-t
    google-translate
    codic ; 変数とかのネーミング（Alt-x codic）; http://futurismo.biz/archives/2538

    ;; window
    e2wm

    ;; mode
    go-mode
    enh-ruby-mode
    nlinum

    ;; for go language
    go-autocomplete
    go-eldoc

    ;; undo/redo
    undo-tree
    undohist
    redo+

    ;; sets of helm
    helm ; 旧anything
;    helm-git
    helm-gtags ; gtags
    yasnippet ; 将来は移植
;    helm-c-yasnippet
    ))

(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/loads/inits/")

;; (provide 'init)
;; ;;; init.el ends here
