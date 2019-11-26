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
;; Install packages list
(defvar installing-package-list
  '(
    init-loader
    dash
    async
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

    ;; sets of helm
    helm ; 旧anything
;    helm-git
    helm-gtags ; gtags
    yasnippet ; 将来は移植
;    helm-c-yasnippet
    )
  "上記に起動時に melpa からインストールしたい Emacs Lisp パッケージを並べる"
  )

(require 'package)
;; package.elでelispを入れるdirectoryの設定
(setq package-user-dir "~/.emacs.d/loads/elisp/elpa/")
(setq package-archives
      '(
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ))
(setq package-pinned-packages
      '(
        (init-loader           . "melpa-stable")
        (dash                  . "melpa-stable")
        (async                 . "melpa-stable")
        (eieio                 . "melpa-stable")
        (auto-complete         . "melpa-stable")
        (fuzzy                 . "melpa-stable")
        (migemo                . "melpa-stable")
        (multiple-cursors      . "melpa-stable")
        (smartrep              . "melpa-stable")
        (expand-region         . "melpa-stable")
        (flycheck              . "melpa-stable")
        (flycheck-pos-tip      . "melpa-stable")
        (exec-path-from-shell  . "melpa-stable")
        (highlight-symbol      . "melpa-stable")
        (auto-highlight-symbol . "marmalade")
        (recentf-ext           . "melpa")
        (smooth-scroll         . "melpa-stable")
        (rainbow-delimiters    . "melpa-stable")
        (color-theme-modern    . "melpa-stable")
        (google-translate      . "melpa-stable")
        (codic                 . "melpa-stable")
        (e2wm                  . "melpa-stable")
        (go-mode               . "melpa-stable")
        (enh-ruby-mode         . "melpa-stable")
        (nlinum                . "gnu")
        (go-autocomplete       . "melpa-stable")
        (go-eldoc              . "melpa-stable")
        (undo-tree             . "marmalade")
        (undohist              . "melpa")
        (helm                  . "melpa-stable")
        (helm-gtags            . "melpa-stable")
        (yasnippet             . "melpa-stable")
        ))
(package-initialize)
;; インストールされていれば、次回以降インストールさせない。
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg installing-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))



(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/loads/inits/")

;; (provide 'init)
;; ;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja")
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(package-selected-packages
   (quote
	(color-theme-modern yasnippet web-mode undohist undo-tree smooth-scroll smartrep redo+ recentf-ext rainbow-delimiters popwin nlinum multiple-cursors migemo js2-mode init-loader highlight-symbol helm-gtags google-translate go-eldoc go-autocomplete fuzzy flycheck-pos-tip expand-region exec-path-from-shell enh-ruby-mode e2wm codic auto-highlight-symbol)))
 '(yas-trigger-key "TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
