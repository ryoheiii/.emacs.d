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
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-enable-imenu-support t)
(eval-when-compile
  (require 'use-package))

;;;;; init-loader
(use-package init-loader
  :ensure t
  :config
  (init-loader-load "~/.emacs.d/loads/inits/"))
(setq init-loader-show-log-after-init nil)

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
