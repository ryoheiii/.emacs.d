;;; Code:
;;; ロードパス
;; ロードパスの追加
 (setq load-path
       (append
        (list
         (expand-file-name "~/.emacs.d/loads/")
         )
        load-path))

;;; 追加の関数定義
;; 便利関数の定義
(load "~/.emacs.d/loads/my-functions/convenience.el")
;; 個別の関数定義があったら読み込む
(load "~/.emacs.d/loads/my-functions/local" t)

;; ~/.emacs.d/elisp 以下全部読み込み
(let ((default-directory (expand-file-name "~/.emacs.d/loads/elisp/elpa/")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;; Package Manegement
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
    ;; Using packages
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
    ;; translate ;; C-c C-t
    google-translate
    popwin
    codic ; 変数とかのネーミング（Alt-x codic）; http://futurismo.biz/archives/2538
    ;; window
    e2wm
    ;; mode
    go-mode
    web-mode
    diff-mode
    enh-ruby-mode
    js2-mode
    yatex
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

(provide 'init)
;;; init.el ends here
