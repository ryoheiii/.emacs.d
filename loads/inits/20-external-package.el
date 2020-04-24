;; https://ainame.hateblo.jp/entry/2013/12/08/162032
(use-package smart-newline
  :ensure t
  :init
  (add-hook 'c++-mode-hook
            '(lambda ()
               (smart-newline-mode 1)))
  (add-hook 'c-mode-hook
            '(lambda ()
               (smart-newline-mode 1)))
  (add-hook 'cc-mode-hook
            '(lambda ()
               (smart-newline-mode 1)))
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (smart-newline-mode 1)))
  (add-hook 'lisp-mode-hook
            '(lambda ()
               (smart-newline-mode 1)))
  )

;;; 単語を検索した時に何番目のマッチかを表示する拡張
;; https://qiita.com/syohex/items/56cf3b7f7d9943f7a7ba
(use-package anzu
  :ensure t
  :init
  (global-anzu-mode 1)
  :config
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-to-string-separator " => ")
   )
  )

(use-package ispell
  :ensure t
  :config
  ;; aspell にパスを通す
  (when (file-executable-p "/usr/bin/aspell")
    (setq-default ispell-program-name "aspell")
    ;; (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
    ;; パフォーマンス向上
    (add-to-list 'ispell-extra-args "--sug-mode=ultra")
    ;; 日本語はスキップ.
    '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

  ;; スペルチェックには英語の辞書を使う
  (setq ispell-dictionary "american")
  )

(use-package irony
  :ensure t
  :init
  :after cc-mode
  :config
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (use-package company-irony-c-headers
    :ensure t
    :after company
    :config
    (add-to-list 'company-backend 'company-ironny-c-headers)
    )
  )


;; C-w でリファレンス表示
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode 1)
  :bind (
         ("C-M-i" . company-complete)
         :map company-mode-map
         ("TAB" . indent-for-tab-command)
         :map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("TAB" . company-complete-selection)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
  (setq company-idle-delay 0) ; デフォルトは 0.5
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 3) ; デフォルトは 4
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (setq company-dabbrev-around t)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-eclim-auto-save nil)
  (setq company-dabbrev-downcase nil)

  ;; yasnippetとの連携
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  ;; color settings
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "grey60")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40")
  )

;;; 自動補完機能-auto-complete ; company に置き換え
(use-package auto-complete
  :ensure t
  :demand t
  :diminish ""
  :bind (:map ac-menu-map
              ("C-n" . ac-next)
              ("C-p" . ac-previous)
              )
  :config
  (use-package auto-complete-c-headers
    :ensure t
    :init
    (add-hook 'c++-mode-hook (lambda ()
                               '(setq ac-sources (append ac-sources '(ac-source-c-headers)))))
    (add-hook 'c-mode-hook (lambda ()
                             '(setq ac-sources (append ac-sources '(ac-source-c-headers)))))
    )

  (use-package auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/my-data/ac-dict") ;; ディレクトリ指定
  (add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
  (ac-set-trigger-key "TAB")

  (setq ac-comphist-file (my-set-history "ac-comphist.dat")) ;; my-set-history @00-auto-file-place.el
  (setq ac-use-menu-map t)
  (setq ac-disable-faces nil) ;;コメントや文字列リテラルでも補完を行う
  (setq ac-use-fuzzy t)       ;; 曖昧マッチ

  ;; yasnippetのbindingを指定するとエラーが出るので回避する方法。
  (setf (symbol-function 'yas-active-keys)
        (lambda ()
          (remove-duplicates (mapcan #'yas--table-all-keys (yas--get-snippet-tables)))))
  )

(use-package e2wm
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-c +") 'e2wm:start-management)

    (setq e2wm:c-code-recipe
          '(| (:left-max-size 30)
              history
              (- (:upper-size-ratio 0.8)
                 main sub)))

    (e2wm:add-keymap
     e2wm:pst-minor-mode-keymap
     '(
       ("<M-left>" . e2wm:dp-code) ; codeへ変更
       ("<M-right>"  . e2wm:dp-two)  ; twoへ変更
       ("<M-up>"    . e2wm:dp-doc)  ; docへ変更
       ("<M-down>"  . e2wm:dp-dashboard) ; dashboardへ変更
       ("C-c C-f"       . e2wm:pst-history-forward-command) ; 履歴を進む
       ("C-c C-b"       . e2wm:pst-history-back-command) ; 履歴をもどる
       ([f5]            . e2wm:dp-code-main-maximize-toggle-command)
                                        ;   ("prefix L"  . ielm)
                                        ;   ("M-m"       . e2wm:pst-window-select-main-command)
       ) e2wm:prefix-key)

    ;; (e2wm:start-management)
    )
  )

;;; キー
;; C-z c    新規スクリーンを作成して移動する elscreen-create
;; C-z k    現在のスクリーンを閉じる         elscreen-kill
;; C-z p    前のスクリーンへ                 elscreen-previous
;; C-z n    次のスクリーンへ                 elscreen-next
;; C-z a    前と次のスクリーンをトグル       elscreen-toggle
;; C-z [0-9]    番号のスクリーンへ           elscreen-jump-[0-9]
;; C-z ?    ヘルプを表示する

;;; helm 連携キー
;; C-z C-f  新しいelscreenでファイルを開く
;; C-z b    新しいelscreenでバッファを開く
;; C-z d    新しいelscreenでdiredを開く
(use-package elscreen
  :ensure t
  :config
  (setq elscreen-prefix-key (kbd "C-z"))
  (elscreen-start)
  ;; タブの先頭に[X]を表示しない
  (setq elscreen-tab-display-kill-screen nil)
  ;; header-lineの先頭に[<->]を表示しない
  (setq elscreen-tab-display-control nil)
  ;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
  (setq elscreen-buffer-to-nickname-alist
        '(("^dired-mode$"     .
           (lambda ()
             (format "Dired(%s)" dired-directory)))
          ("^Info-mode$"      .
           (lambda ()
             (format "Info(%s)" (file-name-nondirectory Info-current-file))))
          ("^mew-draft-mode$" .
           (lambda ()
             (format "Mew(%s)" (buffer-name (current-buffer)))))
          ("^mew-"            . "Mew")
          ("^irchat-"         . "IRChat")
          ("^liece-"          . "Liece")
          ("^lookup-"         . "Lookup")))
  (setq elscreen-mode-to-nickname-alist
        '(("[Ss]hell"         . "shell")
          ("compilation"      . "compile")
          ("-telnet"          . "telnet")
          ("dict"             . "OnlineDict")
          ("*WL:Message*"     . "Wanderlust")))

  ;;;  Use frame-title for tabs
  ;; How to display the list of screens on the frame-title of my Emacs?
  ;; This is broken. get-alist should be changed to alist-get
  ;; https://www.emacswiki.org/emacs/EmacsLispScreen#toc8
  ;;
  (defvar *elscreen-tab-truncate-length*
    20 "Number of characters to truncate tab names in frame title")
  ;;
  (defun elscreen-tabs-as-string ()
    "Return a string representation of elscreen tab names

Set name truncation length in ELSCREEN-TRUNCATE-LENGTH"
    (let* ((screen-list (sort (elscreen-get-screen-list) '<))
           (screen-to-name-alist (elscreen-get-screen-to-name-alist)))
      ;; mapconcat: mapping and then concate name elements together with separator
      (mapconcat
       (lambda (screen)
         (format (if (string-equal "+" (elscreen-status-label screen))
                     ;; Current screen format
                     "[ %d ] %s"
                   ;; Others
                   "(%d) %s")
                 ;; screen number: replaces %d (integer)
                 screen
                 ;; screen name: replaces %s (string)
                 (elscreen-truncate-screen-name
                  ;; Return the value associated with KEY in ALIST
                  (alist-get screen screen-to-name-alist)
                  *elscreen-tab-truncate-length*)))
       ;; Screen numbers (keys for alist)
       screen-list
       ;; Separator
       " | ")))
  ;;
  (defvar *elscreen-tabs-as-string*
    "" "Variable to hold curent elscreen tab names as a string")
  ;;
  (defun update-elscreen-tabs-as-string ()
    "Update *elscreen-tabs-as-string* variable"
    (interactive)
    (setq *elscreen-tabs-as-string* (elscreen-tabs-as-string)))
  ;;
  ;; Update *elscreen-tabs-as-string* whenever elscreen status updates
  (add-hook 'elscreen-screen-update-hook 'update-elscreen-tabs-as-string)
  ;;
  ;; Set frame title format as combination of current elscreen tabs and buffer/path
  (setq frame-title-format '(:eval (concat *elscreen-tabs-as-string*
                                           "    ||    "
                                           (if buffer-file-name
                                               (abbreviate-file-name buffer-file-name)
                                             "%b"))))
  )

(use-package eshell
  :init
  :config
  ;; 確認なしでヒストリ保存
  (setq eshell-ask-to-save-history (quote always))
  ;; 補完時にサイクルする
  (setq eshell-cmpl-cycle-completions nil)
  ;; 補完時に大文字小文字を区別しない
  (setq eshell-cmpl-ignore-case t)

  (setq eshell-save-history-on-exit t)
  (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

  ;;補完候補がこの数値以下だとサイクルせずに候補表示
  ;; (setq eshell-cmpl-cycle-cutoff-length 5)

  ;; 履歴で重複を無視する
  (setq eshell-hist-ignoredups t)

  ;; これで正規表現がつかえるようになる??
  (setq eshell-prompt-regexp "^[^#$]*[$#] ")

  ;; スクロールがカクカクなるのを抑止する.
  ;; http://stackoverflow.com/questions/12667043/emacs-smooth-scrolling-scroll-margin-and-eshell
  ;; (setq scroll-margin 0)

  ;; zsh の環境変数を取り込む
  ;; http://d.hatena.ne.jp/zonu_exe/20120509/1336583187
  ;; (let ((zshpath (shell-command-to-string "/usr/bin/zsh -c 'printenv PATH'")))
  ;;   (let ((pathlst (split-string zshpath ":")))
  ;;     (setq exec-path pathlst))
  ;;   (setq eshell-path-env zshpath)
  ;;   (setenv "PATH" zshpath))
  )

;;; インクリメンタルに選択範囲を広げる
(use-package expand-region
  :ensure t
  :init
  (transient-mark-mode t)
  :bind (
         ("C-,"   . er/expand-region)
         ;; ("C-M-," . er/contract-region)
         )
  :config
  ;; transient-mark-modeが nilでは動作しない
  )

(use-package google-c-style
  :ensure t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (google-set-c-style)
              (google-make-newline-indent)))
  :config
  ;; (c-set-offset 'statement-case-open 0)
  )

(use-package helm
  :ensure t
  :init
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-display-function #'display-buffer)
  (helm-mode 1)
  :bind
  (("M-x"     . 'helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ("C-x C-y" . helm-show-kill-ring)
   ("C-x g"   . helm-do-grep-ag)
   ("C-x b"   . helm-buffers-list)
   ("C-x i"   . helm-imenu)
   :map helm-map
   ("C-h" . delete-backward-char)
   :map helm-find-files-map
   ("C-h" . delete-backward-char)
   ("TAB" . helm-execute-persistent-action)
   :map helm-read-file-map
   ("TAB" . helm-execute-persistent-action))
  :config
  ;; emacsのコマンドを検索可能に
  (defvar helm-source-emacs-commands
    (helm-build-sync-source "Emacs commands"
      :candidates (lambda ()
                    (let ((cmds))
                      (mapatoms
                       (lambda (elt) (when (commandp elt) (push elt cmds))))
                      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "A simple helm source for Emacs commands.")

  ;; emacsのコマンド履歴を検索可能に
  (defvar helm-source-emacs-commands-history
    (helm-build-sync-source "Emacs commands history"
      :candidates (lambda ()
                    (let ((cmds))
                      (dolist (elem extended-command-history)
                        (push (intern elem) cmds))
                      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "Emacs commands history")

  ;; Disable helm in some functions
  ;; (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; (1) helm-buffers-list のバッファ名の領域を広くとる
  (setq helm-buffer-details-flag nil)

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-file-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (setq helm-ff-fuzzy-matching nil)
  (defadvice helm-ff--transform-pattern-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern))))))

  (defun helm-buffers-list-pattern-transformer (pattern)
    (if (equal pattern "")
        pattern
      (let* ((first-char (substring pattern 0 1))
             (pattern (cond ((equal first-char "*")
                             (concat " " pattern))
                            ((equal first-char "=")
                             (concat "*" (substring pattern 1)))
                            (t
                             pattern))))
        ;; Escape some characters
        (setq pattern (replace-regexp-in-string "\\." "\\\\." pattern))
        (setq pattern (replace-regexp-in-string "\\*" "\\\\*" pattern))
        pattern)))
  )

(use-package helm-gtags
  :ensure t
  :init
  (add-hook 'c-mode-hook
            (lambda ()
              (helm-gtags-mode)))
  (add-hook 'c++-mode-hook
            (lambda ()
              (helm-gtags-mode)))
  :bind  (
          ([f11] . helm-gtags-find-tag) ;; 関数の定義場所の検索
          ([f12] . helm-gtags-find-rtag) ;; 関数の使用箇所の検索
          ([f9]  . helm-gtags-find-symbol);; 変数の使用箇所の検索
          ("C-t" . helm-gtags-pop-stack);; gtagsでジャンプする一つ前の状態に戻る
          ([f6]  . helm-gtags-find-files) ;; ファイルジャンプ
          ;; ([f3] . helm-gtags-select)
          ;; ("C-t l" . helm-gtags-show-stack)
          ;; ("C-t m" . helm-gtags-update-tags)
          )
  :config
  (custom-set-variables
   '(helm-gtags-path-style 'root)
   ;; '(helm-gtags-ignore-case t)
   ;; '(helm-gtags-auto-update t)
   )

  ;; auto update GTAGS
  ;; use ~/.globalrc by global
  (defun update-gtags ()
    (interactive)
    (let* ((file (buffer-file-name (current-buffer)))
           (dir (directory-file-name (file-name-directory file))))
      (when (executable-find "global")
        (start-process "gtags-update" nil "global" "-uv")
        )
      )
    )
  ;; (add-hook 'after-save-hook 'update-gtags)
  )

;; helm-M-xでキーバインドを表示してくれる
(use-package helm-descbinds
  :ensure t
  :init
  (helm-descbinds-mode 1)
  )

;; helmで高速ファイル中身サーチ(ag)
(use-package helm-ag
  :ensure t
  )

(use-package highlight-symbol
  :ensure t
  :bind (
         ([f3] . highlight-symbol-at-point)
         ([f4] . highlight-symbol-remove-all)
         )
  :config
  ;; 使いたい色を設定、repeat
  (setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))
  )

(use-package auto-highlight-symbol
  :ensure t
  :config
  (global-auto-highlight-symbol-mode t)
  ;; memo
  ;; C-x C-aで一括rename
  )

(use-package imenu-list
  :ensure t
  )

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 30) ;; minibufferのサイズを拡大！（重要）
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
  ;; (fset 'ivy--regex 'identity)
  )

;; ;; 参考 http://rubikitch.com/2014/08/20/migemo/
(use-package migemo
  :ensure t
  :config
  ;; Set your installed path
  (setq migemo-command "cmigemo"
        migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (setq migemo-options '("-q" "--emacs")
        migemo-user-dictionary nil
        migemo-regex-dictionary nil
        migemo-coding-system 'utf-8-unix)
  (migemo-init)
  (use-package helm
    :init
    (helm-migemo-mode 1)
    )
  )

(use-package avy-migemo
  :ensure t
  :init
  (avy-migemo-mode 1)
  :config
  (setq avy-timeout-seconds nil)
  ;; (use-package avy-migemo-e.g.swiper :ensure t)
  (global-set-key (kbd "C-M-;") 'avy-migemo-goto-char-timer)
  ;;  (global-set-key (kbd "M-g m m") 'avy-migemo-mode)
  )

(use-package move-text
  :ensure t
  :bind (
         ("C-M-p" . move-text-up)
         ("C-M-n" . move-text-down)
         )
  )

(use-package mozc
  :ensure t
  :config
  (prefer-coding-system 'utf-8)
  )

(use-package codic
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/list-file (my-set-history "mc-lists.el"))  ;; my-set-history @00-auto-file-place.el

  (global-set-key (kbd "C-M-c") 'mc/edit-lines)
  (global-set-key (kbd "C-*")   'mc/mark-all-like-this)

  (global-unset-key "\C-q")
  (use-package smartrep :ensure t)
  (declare-function smartrep-define-key "smartrep")
  (smartrep-define-key global-map "C-q"
    '(("p"      . 'mc/mark-previous-like-this)
      ("n"      . 'mc/mark-next-like-this)
      ("*"      . 'mc/mark-all-like-this)
      ("d"      . 'mc/mark-all-like-this-dwim)
      ("m"      . 'mc/mark-more-like-this-extended)
      ("u"      . 'mc/unmark-next-like-this)
      ("U"      . 'mc/unmark-previous-like-this)
      ("s"      . 'mc/skip-to-next-like-this)
      ("S"      . 'mc/skip-to-previous-like-this)
      ("i"      . 'mc/insert-numbers)
      ("o"      . 'mc/sort-regions)
      ("O"      . 'mc/reverse-regions)))
  ;; smartrepによるコマンド実行中はキー入力をエコーしない
  ;; http://shakenbu.org/yanagi/d/?date=20140105
  (advice-add 'smartrep-map-internal
              :around (lambda (orig-fun &rest args)
                        (let ((echo-keystrokes 0))
                          (apply orig-fun args))))
  )

;; 左側にでるファイラー
(use-package neotree
  :ensure t
  :init
  (setq-default neo-keymap-style 'concise)
  :bind (
         ([f8] . neotree-toggle)
         )
  :config
  (setq neo-smart-open t) ;ウインドウを開くたびにcurrent fileのあるディレクトリを表示
  (setq neo-create-file-auto-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-show-hidden-files t)

  ;;(setq neo-vc-integration '(face char))
  (setq neo-toggle-window-keep-p t)
  (bind-key [f8] 'neotree-toggle)
  (bind-key "C-c c" 'neotree-create-node neotree-mode-map)
  (bind-key "C-c d" 'neotree-delete-node neotree-mode-map)
  (bind-key "C-c r" 'neotree-rename-node neotree-mode-map)
  (bind-key "C-c p" 'neotree-copy-node neotree-mode-map)
  (bind-key "C-c s" 'neotree-stretch-toggle neotree-mode-map)
  (bind-key "C-c t" 'neotree-toggle neotree-mode-map)
  (bind-key "C-c n" 'neotree-create-node neotree-mode-map)
  (bind-key* "C-x n" 'neotree-refresh) ;バッファで開いているところをneoteeのルートにする

  (neotree)

  ;; Change neotree's font size
  ;; Tips from https://github.com/jaypei/emacs-neotree/issues/218
  (defun neotree-text-scale ()
    "Text scale for neotree."
    (interactive)
    (text-scale-adjust 0)
    (text-scale-decrease 1)
    (message nil))
  (add-hook 'neo-after-create-hook
            (lambda (_)
              (call-interactively 'neotree-text-scale)))

  ;; neotree enter hide
  ;; Tips from https://github.com/jaypei/emacs-neotree/issues/77
  (defun neo-open-file-hide (full-path &optional arg)
    "Open file and hiding neotree.
The description of FULL-PATH & ARG is in `neotree-enter'."
    (neo-global--select-mru-window arg)
    (find-file full-path)
    (neotree-hide))

  (defun neotree-enter-hide (&optional arg)
    "Neo-open-file-hide if file, Neo-open-dir if dir.
The description of ARG is in `neo-buffer--execute'."
    (interactive "P")
    (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))
  )

(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  :config
  (defmacro with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))

  ;; (setq recentf-save-file "~/.emacs.d/hist/recentf" "-" user-full-name)
  (setq recentf-max-saved-items 2000 ;; recentf に保存するファイルの数
        recentf-max-menu-items 15
        ;; .recentf自体は含まない
        recentf-exclude '("recentf" "-" user-full-name)
        ;; my-set-history @00-auto-file-place.el
        recentf-save-file (my-set-history "recentf" "-" user-full-name)
        ;; 保存する内容を整理
        recentf-auto-cleanup 'never
        recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list)
   )
  )

(use-package recentf-ext
  :ensure t
  )

;; C-vとかのスクロールがスムーズになる
(use-package smooth-scroll
  :ensure t
  :config
  (smooth-scroll-mode t)
  (setq smooth-scroll/vscroll-step-size 8)
  )

;;インクリメンタルバッファサーチ
(use-package swiper
  :ensure t
  :bind (
         ("C-s" . swiper)
         )
  :config
  )

;; C-/でundo
;; C-x uで樹形図表示
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :config
  (defun undo-tree-split-side-by-side (original-function &rest args)
    "Split undo-tree side-by-side"
    (let ((split-height-threshold nil)
          (split-width-threshold 0))
      (apply original-function args)))
  (advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side)
  ;; visualizerはRETかC-gで終了
  (define-key undo-tree-visualizer-mode-map (kbd "RET") 'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-quit)
  )

(use-package undohist
  :ensure t
  :config
  (setq undohist-directory (my-set-history "undohist")) ;; my-set-history @00-auto-file-place.el
  (undohist-initialize)
  )

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  )

;;; snippet系
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs '(
                           "~/.emacs.d/my-data/snippets"
                           "~/.emacs.d/loads/elisp/yasnippet-snippets-0.22/snippets"
                           ))
  ;; (yas-reload-all)
  (yas-global-mode 1)
  :diminish
  ;; :bind (:map yas-minor-mode-map
  ;;             ("C-x i i" . yas-insert-snippet)     ;; 既存スニペットを挿入する
  ;;             ("C-x i n" . yas-new-snippet)        ;; 新規スニペットを作成するバッファを用意する
  ;;             ("C-x i v" . yas-visit-snippet-file) ;; 既存スニペットを閲覧・編集する
  ;;             ("C-x i l" . yas-describe-tables)
  ;;             ("C-x i g" . yas-reload-all)
  ;;             )
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (setq yas-prompt-functions '(yas-ido-prompt))
  (custom-set-variables '(yas-trigger-key "TAB"))

  ;; (use-package ivy-yasnippet
  ;;   :ensure t
  ;;   :bind (
  ;;          ("C-c C-y" . ivy-yasnippet)
  ;;          )
  ;;   :config
  ;;   (setq ivy-yasnippet-expand-keys "smart")
  ;;   (advice-add #'ivy-yasnippet--preview :override #'ignore)
  ;;   )

  (use-package helm-c-yasnippet
    :ensure t
    :bind (
           ("C-c y" . helm-yas-complete)
           )
    :config
    (setq helm-yas-space-match-any-greedy t)
    )
  )
