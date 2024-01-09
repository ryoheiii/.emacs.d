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
      '(("gnu"          . "https://elpa.gnu.org/packages/")     ; GNU の公式パッケージ
        ("melpa"        . "https://melpa.org/packages/")        ; 最大でもっとも更新が頻繁なパッケージ
        ("melpa-stable" . "https://stable.melpa.org/packages/") ; MELPA の安定版
        ;; ("org"          . "https://orgmode.org/elpa/")       ; Org の公式パッケージ
        ))
;; パッケージアーカイブの優先度の設定（高い数値が高い優先度）
(setq package-archive-priorities
      '(("gnu"          . 3)
        ("melpa-stable" . 2)
        ("melpa"        . 1)))
(setq package-pinned-packages
      '((ace-window                . "melpa-stable")
        (aggressive-indent         . "melpa-stable")
        (amx                       . "melpa-stable")
        (anzu                      . "melpa-stable")
        (async                     . "melpa-stable")
        (auto-complete             . "melpa-stable")
        (auto-complete-c-headers   . "melpa")
        (auto-complete-clang-async . "melpa-stable")
        (auto-highlight-symbol     . "melpa-stable")
        (avy                       . "melpa-stable")
        (avy-migemo                . "melpa-stable")
        (beacon                    . "melpa-stable")
        (bind-key                  . "melpa-stable")
        (codic                     . "melpa-stable")
        (color-theme-modern        . "melpa-stable")
        (color-theme-modern        . "melpa-stable") ;; for hober color-theme
        (company                   . "melpa-stable")
        (company-box               . "melpa")
        (company-c-headers         . "melpa-stable")
        (company-fuzzy             . "melpa-stable")
        (company-irony             . "melpa-stable")
        (company-irony-c-headers   . "melpa-stable")
        (company-posframe          . "melpa-stable")
        (company-statistics        . "melpa-stable")
        (consult                   . "melpa-stable")
        (dash                      . "melpa") ;; for git-gutter+
        (dashboard                 . "melpa-stable")
        (dimmer                    . "melpa-stable")
        (doom-modeline             . "melpa-stable")
        (doom-themes               . "melpa-stable")
        (dumb-jump                 . "melpa-stable")
        (e2wm                      . "melpa-stable")
        (easy-kill                 . "melpa-stable")
        (eieio                     . "melpa-stable")
        (eshell                    . "melpa-stable")
        (exec-path-from-shell      . "melpa-stable")
        (expand-region             . "melpa-stable")
        (fill-column-indicator     . "melpa-stable")
        (flycheck                  . "melpa-stable")
        (flycheck-pos-tip          . "melpa-stable")
        (free-key                  . "melpa-stable")
        (fuzzy                     . "melpa-stable")
        (gcmh                      . "gnu")
        (git-gutter+               . "melpa-stable")
        (go-autocomplete           . "melpa-stable")
        (go-eldoc                  . "melpa-stable")
        (go-mode                   . "melpa-stable")
        (golden-ratio              . "melpa-stable")
        (google-c-style            . "melpa")
        (google-translate          . "melpa-stable")
        (helm                      . "melpa-stable")
        (helm-ag                   . "melpa-stable")
        (helm-c-yasnippet          . "melpa-stable")
        (helm-core                 . "melpa-stable")
        (helm-descbinds            . "melpa-stable")
        (helm-gtags                . "melpa-stable")
        (hide-mode-line            . "melpa-stable")
        (highlight-indent-guides   . "melpa")
        (highlight-symbol          . "melpa-stable")
        (imenu-list                . "melpa-stable")
        (init-loader               . "melpa-stable")
        (irony                     . "melpa-stable")
        (ispell                    . "melpa-stable")
        (ivy                       . "melpa-stable")
        (ivy-migemo                . "melpa-stable")
        (ivy-yasnippet             . "melpa-stable")
        (marginalia                . "melpa-stable")
        (migemo                    . "melpa-stable")
        (minimap                   . "gnu")
        (move-text                 . "melpa-stable")
        (mozc                      . "melpa")
        (multiple-cursors          . "melpa-stable")
        (neotree                   . "melpa-stable")
        (nlinum                    . "melpa-stable")
        (orderless                 . "melpa-stable")
        (org-pomodoro              . "melpa-stable")
        (paradox                   . "melpa-stable")
        (popup                     . "melpa-stable")
        (popwin                    . "melpa-stable")
        (powerline                 . "melpa-stable")
        (rainbow-delimiters        . "melpa-stable")
        (recentf                   . "melpa")
        (recentf-ext               . "melpa")
        (region-bindings-mode      . "melpa")
        (s                         . "melpa-stable")
        (smart-hungry-delete       . "melpa")
        (smart-mode-line           . "melpa-stable")
        (smart-newline             . "melpa")
        (smartrep                  . "melpa-stable")
        (smooth-scroll             . "melpa-stable")
        (srefactor                 . "melpa-stable")
        (swiper                    . "melpa-stable")
        (swiper-helm               . "melpa-stable")
        (tabbar                    . "melpa-stable")
        (total-lines               . "melpa-stable")
        (undo-tree                 . "gnu")
        (undohist                  . "melpa-stable")
        (use-package               . "melpa-stable")
        (vertico                   . "melpa-stable")
        (volatile-highlights       . "melpa-stable")
        (which-key                 . "melpa-stable")
        (window-layout             . "melpa-stable")
        (xclip                     . "gnu")
        (yasnippet                 . "melpa-stable")
        (yasnippet-snippets        . "melpa-stable")
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
