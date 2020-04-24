;; (use-package doom-themes
;;   :ensure t
;;   :init
;;   (load-theme 'doom-dracula t)
;;   :custom
;;   (doom-themes-enable-italic t)
;;   (doom-themes-enable-bold t)
;;   :custom-face
;;   (doom-modeline-bar ((t (:background "#6272a4"))))
;;   :config
;;   (doom-themes-neotree-config)
;;   (doom-themes-org-config)
;;   )

;; (use-package xclip
;;   :ensure t
;;   :init
;;   (xclip-mode 1)
;;   )

;; ;;; 分割ウインドウをいい感じの比率で制御
;; (use-package golden-ratio
;;   :ensure t
;;   :init (golden-ratio-mode t)
;;   :config
;;   ;; NeoTree との干渉に対する例外処理
;;   (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
;;  )

;; ;; M-w   行
;; ;; M-w w 単語
;; ;; M-w s S式
;; ;; M-w l リスト
;; ;; M-w f ファイル名
;; ;; M-w d defun
;; ;; M-w D 関数名
;; ;; M-w e 行
;; (use-package easy-kill
;;   :ensure t
;;   :config
;;   (global-set-key [remap kill-ring-save] 'easy-kill)
;;   )

;; (use-package dumb-jump
;;   :ensure t
;;   :config
;;   ;; これをしないとホームディレクトリ以下が検索対象になる
;;   (setq dumb-jump-default-project "")
;;   ;; 日本語を含むパスだとgit grepがちゃんと動かない…
;;   (setq dumb-jump-force-searcher 'rg)
;;   ;; 標準キーバインドを有効にする
;;   (setq dumb-jump-mode t)
;;   (setf dumb-jump-selector 'helm)
;;   ;; (setq dumb-jump-selector 'ivy) ;; 候補選択をivyに任せます
;;   (setq dumb-jump-use-visible-window nil)

;;   (define-key global-map (kbd "C-c j") 'dumb-jump-go) ;; go-to-definition!
;;   (define-key global-map [(super shift d)] 'dumb-jump-back)
;;   )

;; ;;PATHをシェルから引き継ぐ
;; (use-package exec-path-from-shell
;;   :ensure t
;;   :init (exec-path-from-shell-initialize)
;;   )

;; ;; モードラインがきれいになる
;; (use-package powerline
;;   :ensure t
;;   :config
;;   (defun shorten-directory (dir max-length)
;;     "Show up to `max-length' characters of a directory name `dir'."
;;     (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
;;           (output ""))
;;       (when (and path (equal "" (car path)))
;;         (setq path (cdr path)))
;;       (while (and path (< (length output) (- max-length 4)))
;;         (setq output (concat (car path) "/" output))
;;         (setq path (cdr path)))
;;       (when path
;;         (setq output (concat ".../" output)))
;;       output))

;;   (defun powerline-my-theme ()
;;     "Setup the my mode-line."
;;     (interactive)
;;     (setq-default mode-line-format
;;                   '("%e"
;;                     (:eval
;;                      (let* ((active (powerline-selected-window-active))
;;                             (mode-line (if active 'mode-line 'mode-line-inactive))
;;                             (face1 (if active 'powerline-active1 'powerline-inactive1))
;;                             (face2 (if active 'powerline-active2 'powerline-inactive2))
;;                             (separator-left (intern (format "powerline-%s-%s"
;;                                                             powerline-default-separator
;;                                                             (car powerline-default-separator-dir))))
;;                             (separator-right (intern (format "powerline-%s-%s"
;;                                                              powerline-default-separator
;;                                                              (cdr powerline-default-separator-dir))))
;;                             (lhs (list (powerline-raw "%*" nil 'l)
;;                                        ;;                                       (powerline-buffer-size nil 'l)
;;                                        ;;                                       (powerline-raw mode-line-mule-info nil 'l)
;;                                        (powerline-raw
;;                                         (shorten-directory default-directory 15)
;;                                         nil 'l)
;;                                        (powerline-buffer-id nil 'r)
;;                                        (when (and (boundp 'which-func-mode) which-func-mode)
;;                                          (powerline-raw which-func-format nil 'l))
;;                                        (powerline-raw " ")
;;                                        (funcall separator-left mode-line face1)
;;                                        (when (boundp 'erc-modified-channels-object)
;;                                          (powerline-raw erc-modified-channels-object face1 'l))
;;                                        (powerline-vc face1 'r)
;;                                        (powerline-major-mode face1 'l)
;;                                        (powerline-process face1)
;;                                         ;(powerline-minor-modes face1 'l)
;;                                        (powerline-narrow face1 'l)
;;                                        (powerline-raw " " face1)
;;                                        (funcall separator-left face1 face2)
;;                                        ))
;;                             (rhs (list (powerline-raw global-mode-string face2 'r)
;;                                        (funcall separator-right face2 face1)
;;                                        (powerline-raw "%4l" face1 'l)
;;                                        (powerline-raw ":" face1 'l)
;;                                        (powerline-raw "%3c" face1 'r)
;;                                        (funcall separator-right face1 mode-line)
;;                                        (powerline-raw " ")
;;                                        (powerline-raw "%6p" nil 'r)
;;                                        (powerline-hud face2 face1))))
;;                        (concat (powerline-render lhs)
;;                                (powerline-fill face2 (powerline-width rhs))
;;                                (powerline-render rhs)))))))
;;   (powerline-my-theme)
;;   ;;(setq-default mode-line-format nil)
;;   ;; powerlineの色を変えたい時はここ
;;   ;;(defun make/set-face (face-name fg-color bg-color weight)
;;   ;;  (make-face face-name)
;;   ;;  (set-face-attribute face-name nil
;;   ;;                      :foreground fg-color :background bg-color :box nil :weight weight))
;;   ;;(make/set-face 'mode-line-1-fg "#282C34" "#9b7cb6" 'bold)
;;   ;;(make/set-face 'mode-line-2-fg "#AAAAAA" "#2F343D" 'bold)
;;   ;;(make/set-face 'mode-line-1-arrow  "#AAAAAA" "#9b7cb6" 'bold)
;;   ;;(make/set-face 'mode-line-2-arrow  "#AAAAAA" "#3E4451" 'bold)
;;   )

;; (use-package swiper-helm
;;   :ensure t
;;   :commands (swiper
;;              swiper-helm
;;              swiper-helm-at-point
;;              swiper-helm-from-isearch)
;;   :bind (("s-s" . swiper-helm-at-point)
;;          ("C-s-s" . swiper-helm))
;;   :bind (:map isearch-mode-map
;;               ("s-s" . swiper-helm-from-isearch))
;;   ;; Configuration
;;   :config
;;   ;; Newly defined
;;   (defun swiper-helm-at-point ()
;;     "Custom function to pick up a thing at a point for swiper-helm

;; If there is a symbol at the current point, its textual representation is
;; searched for by swiper-helm. If there is no symbol, empty search box is
;; started."
;;     (interactive)
;;     (swiper-helm (cond
;;                   ;; If there is selection use it
;;                   ((and transient-mark-mode mark-active (not (eq (mark) (point))))
;;                    (buffer-substring-no-properties (mark) (point)))
;;                   ;; Otherwise, use symbol at point or empty
;;                   (t (format "%s"
;;                              (or (thing-at-point 'symbol)
;;                                  ""))))))
;;   )

;; ;;;; semantic
;; ;; C/C++等の構文を静的に解析していろいろな機能を提供するsemantic
;; ;;;; srefactor
;; ;; リファクタリングツール
;; (use-package srefactor
;;   :ensure t
;;   :defer  t
;;   :config
;;   (semantic-mode 0)
;;   (define-key c-mode-map (kbd "C-c r") 'srefactor-refactor-at-point)
;;   (define-key c++-mode-map (kbd "C-c r") 'srefactor-refactor-at-point)
;;   )
