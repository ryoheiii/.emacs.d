;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688
(use-package helm
  :ensure t
  :init
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-display-function #'display-buffer)
  :bind
  (("M-x" . 'helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ("C-x C-y" . helm-show-kill-ring)
   ("C-x C-b" . helm-buffers-list)
   ("C-x C-i" . helm-imenu)
   :map helm-map
   ("C-h" . delete-backward-char)
   :map helm-find-files-map
   ("C-h" . delete-backward-char)
   ("TAB" . helm-execute-persistent-action)
   :map helm-read-file-map
   ("TAB" . helm-execute-persistent-action))
  :config
  (helm-mode 1)

  ;; Disable helm in some functions
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

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
  :bind
  (([f11] . helm-gtags-find-tag) ;; 関数の定義場所の検索
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

(use-package recentf
  :ensure t
  :config
  (defmacro with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))

  ;; (setq recentf-save-file "~/.emacs.d/hist/recentf" "-" user-full-name)
  (setq recentf-max-saved-items 2000)                    ;; recentf に保存するファイルの数
  (setq recentf-exclude '("recentf" "-" user-full-name)) ;; .recentf自体は含まない
  (setq recentf-auto-cleanup 'never)                     ;; 保存する内容を整理
  (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  ;; (run-with-idle-timer 30 t '(lambda ()                  ;; 30秒ごとに .recentf を保存
  ;;                              (with-suppressed-message (recentf-save-list))))
  )

(use-package recentf-ext
  :ensure t)

