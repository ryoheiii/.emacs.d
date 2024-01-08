;;; Code:

;;; ロードパスの設定
;; load-path の追加関数
(defun add-to-load-path (&rest paths)
  "Add specified PATHS to the Emacs load-path."
  (dolist (path paths paths)
    (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
      (add-to-list 'load-path default-directory)
      (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path)))))

;; load-path に追加するディレクトリ
(add-to-load-path "loads/elisp/" "loads/site-elisp/")

;;; パッケージの設定
(require 'package)
(setq package-user-dir (expand-file-name "loads/elisp/" user-emacs-directory))
;; パッケージアーカイブの設定
(setq package-archives
      '(
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ;;("marmalade"    . "https://marmalade-repo.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")
        ))
(setq package-pinned-packages
      '(
        (init-loader               . "melpa-stable")
        (use-package               . "melpa-stable")
        (bind-key                  . "melpa-stable")
        (smart-mode-line           . "melpa-stable")
        (smart-hungry-delete       . "melpa")
        (total-lines               . "melpa-stable")
        (hide-mode-line            . "melpa-stable")
        (dimmer                    . "melpa-stable")
        (ace-window                . "melpa-stable")
        (minimap                   . "gnu")
        (fill-column-indicator     . "melpa-stable")
        (git-gutter+               . "melpa-stable")
        (beacon                    . "melpa-stable")
        (volatile-highlights       . "melpa-stable")
        (highlight-indent-guides   . "melpa")
        (dashboard                 . "melpa-stable")
        (amx                       . "melpa-stable")
        (s                         . "melpa-stable")
        (org-pomodoro              . "melpa-stable")
        (doom-modeline             . "melpa-stable")
        (avy                       . "melpa-stable")
        (xclip                     . "gnu")
        (irony                     . "melpa-stable")
        (doom-themes               . "melpa-stable")
        (golden-ratio              . "melpa-stable")
        (srefactor                 . "melpa-stable")
        (company                   . "melpa-stable")
        (company-statistics        . "melpa-stable")
        (company-irony             . "melpa-stable")
        (company-irony-c-headers   . "melpa-stable")
        (company-c-headers         . "melpa-stable")
        (company-fuzzy             . "melpa-stable")
        (smart-newline             . "melpa")
        (anzu                      . "melpa-stable")
        (popup                     . "melpa-stable")
        (swiper-helm               . "melpa-stable")
        (dumb-jump                 . "melpa-stable")
        (avy-migemo                . "melpa-stable")
        (ivy                       . "melpa-stable")
        (ivy-migemo                . "melpa-stable")
        (ivy-yasnippet             . "melpa-stable")
        (eshell                    . "melpa-stable")
        (auto-complete             . "melpa-stable")
        (auto-complete-c-headers   . "melpa")
        (auto-complete-clang-async . "melpa-stable")
        (mozc                      . "melpa")
        (google-translate          . "melpa-stable")
        (swiper                    . "melpa-stable")
        (exec-path-from-shell      . "melpa-stable")
        (tabbar                    . "melpa-stable")
        (which-key                 . "melpa-stable")
        (neotree                   . "melpa-stable")
        (imenu-list                . "melpa-stable")
        (ispell                    . "melpa-stable")
        (powerline                 . "melpa-stable")
        (easy-kill                 . "melpa-stable")
        (dash                      . "melpa") ;; for git-gutter+
        (async                     . "melpa-stable")
        (eieio                     . "melpa-stable")
        (fuzzy                     . "melpa-stable")
        (move-text                 . "melpa-stable")
        (migemo                    . "melpa-stable")
        (multiple-cursors          . "melpa-stable")
        (google-c-style            . "melpa")
        (smartrep                  . "melpa-stable")
        (expand-region             . "melpa-stable")
        (flycheck                  . "melpa-stable")
        (flycheck-pos-tip          . "melpa-stable")
        (highlight-symbol          . "melpa-stable")
        (auto-highlight-symbol     . "melpa-stable")
        (recentf                   . "melpa")
        (recentf-ext               . "melpa")
        (smooth-scroll             . "melpa-stable")
        (rainbow-delimiters        . "melpa-stable")
        (region-bindings-mode      . "melpa")
        (color-theme-modern        . "melpa-stable")
        (google-translate          . "melpa-stable")
        (codic                     . "melpa-stable")
        (e2wm                      . "melpa-stable")
        (go-mode                   . "melpa-stable")
        (nlinum                    . "melpa-stable")
        (go-autocomplete           . "melpa-stable")
        (go-eldoc                  . "melpa-stable")
        (undo-tree                 . "gnu")
        (undohist                  . "melpa-stable")
        (helm                      . "melpa-stable")
        (window-layout             . "melpa-stable")
        (helm-core                 . "melpa-stable")
        (helm-gtags                . "melpa-stable")
        (helm-descbinds            . "melpa-stable")
        (helm-ag                   . "melpa-stable")
        (helm-c-yasnippet          . "melpa-stable")
        (yasnippet                 . "melpa-stable")
        (yasnippet-snippets        . "melpa-stable")
        (color-theme-modern        . "melpa-stable") ;; for hober color-theme
        (Aggressive-indent         . "melpa-stable")
        (vertico                   . "melpa-stable")
        (consult                   . "melpa-stable")
        (orderless                 . "melpa-stable")
        (marginalia                . "melpa-stable")
        ))
;; パッケージシステムの初期化
(package-initialize)

;; パッケージリストの更新と use-package の確保
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package の設定
(eval-when-compile
  (require 'use-package))

;;; straight.el の設定
(defvar straight-base-dir (expand-file-name "loads/elisp/" user-emacs-directory))
(setq straight-repository-branch "develop")

;; straight.el 自身のインストールと初期設定
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;; Init-loader - 設定ファイルを整理し、分割された設定を読み込む
(use-package init-loader
  :ensure t
  :config
  (init-loader-load "~/.emacs.d/loads/inits/")
  (setq init-loader-show-log-after-init nil)
  )

(provide 'init)
;;; init.el ends here
