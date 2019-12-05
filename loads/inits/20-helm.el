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
   ("C-x C-b" . helm-imenu)
   :map helm-map
   ("C-h" . delete-backward-char)
   :map helm-find-files-map
   ("C-h" . delete-backward-char)
   ("TAB" . helm-execute-persistent-action)
   :map helm-read-file-map
   ("TAB" . helm-execute-persistent-action))
  :config
  (helm-mode 1))

  ;; Disable helm in some functions
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
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
                      (concat ".*" input-pattern)))))))

;; https://emacs-jp.github.io/packages/helm/helm-gtags.html
(use-package helm-gtags
  :ensure t)

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'perl-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)
(add-hook 'clojure-mode-hook 'helm-gtags-mode)
(add-hook 'erlang-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
             ;; 指定した関数が定義されている部分をさがす
             (local-set-key [f11] 'helm-gtags-find-tag)
;              (local-set-key kbd "M-t" 'helm-gtags-find-tag)
             ;; 指定した関数が参照されている部分を探す
             (local-set-key [f12] 'helm-gtags-find-rtag)
;             (local-set-key "M-r" 'helm-gtags-find-rtag)
             ;; 指定した変数、定義の定義元を探す
             (local-set-key [f9] 'helm-gtags-find-symbol)
;              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
             ;; gtagsでジャンプする一つ前の状態に戻る
             (local-set-key "\C-t" 'helm-gtags-pop-stack)

             (local-set-key [f6] 'helm-gtags-find-files)
             ))
