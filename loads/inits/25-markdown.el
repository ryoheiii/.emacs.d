;;; 25-markdown.el --- Markdown 関連の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Markdown モードとその関連パッケージの設定
;; カスタム関数群は loads/site-elisp/my-markdown.el に定義

;;; Code:

;;;;; [Group] Markdown - Markdown 関連 ;;;;;
;;; markdown-mode - markdown mode の設定
(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . markdown-mode)
  :init (setq markdown-enable-math t                   ; 数式 ($…$ / $$…$$) を有効に
              markdown-fontify-code-blocks-natively t) ; コードブロックを必ず着色
  :hook ((markdown-mode . my/markdown-setup))
  :custom
  (markdown-indent-level 4)
  (markdown-link-space-substitution-method 'underscores) ; リンクのスペースをアンダースコアに置換
  (markdown-header-scaling t)                            ; 見出しサイズの自動調整
  ;; コードブロックのシンタックスハイライト
  (markdown-code-lang-modes '(("bash"   . shell-script)
                              ("elisp"  . emacs-lisp)
                              ("python" . python)))
  ;; 画像を表示する設定
  (markdown-image-use-cache t) ; キャッシュして表示
  :bind (("C-c C-v h" . markdown-insert-header-dwim)     ; 見出しを挿入
         ("C-c C-v l" . markdown-insert-link)            ; リンクを挿入
         ("C-c C-v c" . markdown-insert-gfm-code-block)  ; コードブロックを挿入
         ("C-c C-v d" . markdown-insert-details))        ; 折り畳み項目を挿入
  :config
  (require 'my-markdown)
  ;; Pandoc コマンドと CSS を遅延設定（markdown-mode ロード時に初めて構築）
  (setq markdown-command (my-markdown-pandoc-command))
  (setq markdown-export-command (my-markdown-pandoc-command))
  (setq markdown-xhtml-header-content
        (format "<meta charset='utf-8'>\n
                <meta name='viewport' content='width=device-width, initial-scale=1'>\n
                <title>Markdown Export</title>\n
                <style>\n%s\n</style>\n
                <script src='https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.0/highlight.min.js'></script>\n
                <script>hljs.configure({languages: []});hljs.highlightAll();</script>\n"
                (my-markdown-load-css)))
  )

;;; markdown-toc - 目次生成
(use-package markdown-toc
  :straight t
  :after markdown-mode
  :hook ((markdown-mode . markdown-toc-mode)          ; テーブルの自動整形
         ;; (markdown-mode . markdown-toc-insert-toc) ; 保存時に目次を自動挿入
         ;; (before-save . markdown-toc-update-toc)   ; 保存前に目次を更新
         )
  :bind (("C-c C-v t" . markdown-toc-generate-toc))   ; 目次を手動で生成
  :custom
  (markdown-toc-without-markdown-toc t)               ; コメントを含めない
  (markdown-toc-headline-style 'atx)                  ; 見出しのスタイル
  )

;;; pandoc-mode - Markdown を他の形式に変換
(use-package pandoc-mode
  :straight t
  :commands (pandoc-mode)
  :hook (markdown-mode . pandoc-mode)
  )

(provide '25-markdown)
;;; 25-markdown.el ends here
