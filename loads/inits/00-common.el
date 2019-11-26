;;;文字コードの設定
(set-language-environment       'Japanese)
(prefer-coding-system           'utf-8)

;;; Macで日本語のファイル名を扱う場合の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;;; window size
(if window-system (progn
  (setq initial-frame-alist '((width . 176)(height . 53)(top . 0)(left . 0)))
  ;; (set-background-color "Black")
  ;; (set-foreground-color "White")
  ;; (set-cursor-color "Gray")
))

;;; grep
(define-key global-map (kbd "C-c g") 'grep)
(require 'grep)
(setq grep-command-before-query "grep -nri -e ")
(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
             (concat grep-command-before-query
                     (shell-quote-argument (grep-tag-default)))))
        (cons (if buffer-file-name
                  (concat grep-command-before-target
                          " *."
                          (file-name-extension buffer-file-name))
                (concat grep-command-before-target " ."))
              (+ (length grep-command-before-target) 1)))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
                         (+ (length grep-command-before-query) 1)))

;;; 選択範囲をisearch
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;;; 日本語環境
;; Localeに合わせた環境の設定
(set-locale-environment nil)

;;; バー
;; メニューバーを消す
(menu-bar-mode -1)
;; ツールバーを消す
(tool-bar-mode -1)

;;; カーソル
;; カーソルの点滅を止める
(blink-cursor-mode 0)

;;; eval
;; evalした結果を全部表示
(setq eval-expression-print-length nil)

;;; 括弧
;; 対応する括弧を光らせる。
(show-paren-mode 1)
;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
(setq show-paren-style 'mixed)
;; 括弧の範囲色

;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;; カーソルの位置が何行目かを表示する
(line-number-mode t)
;; カーソルの場所を保存する
(require 'saveplace)
(setq-default save-place t)

;;; 行
;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)
;; ;; 最終行に必ず一行挿入する
; (setq require-final-newline t)
(setq require-final-newline nil)
;; ;; バッファの最後でnewlineで新規行を追加するのを禁止する
;; (setq next-line-add-newlines nil)

;;; バックアップ
;; バックアップファイルを作らない
(setq backup-inhibited t)
;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;;; 補完
;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; 部分一致の補完機能を使う
;; p-bでprint-bufferとか
;; 2012-08-08
;; Emacs 24ではデフォルトで有効になっていて、`partial-completion-mode'は
;; なくなっている。カスタマイズする場合は以下の変数を変更する。
;;   * `completion-styles'
;;   * `completion-pcm-complete-word-inserts-delimiters'
(if (fboundp 'partial-completion-mode)
    (partial-completion-mode t))
;; 補完可能なものを随時表示
;; 少しうるさい
(icomplete-mode 1)

;;; 履歴
;; 履歴数を大きくする
(setq history-length 10000)
;; ミニバッファの履歴を保存する
(savehist-mode 1)

;;; バッファ名
;; ファイル名が重複していたらディレクトリ名を追加する。
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; shebangがあるファイルを保存すると実行権をつける。
;; 2012-03-15
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; リージョンの大文字小文字変換を有効にする。
;; C-x C-u -> upcase
;; C-x C-l -> downcase
;; 2011-03-09
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; 現在の関数名をウィンドウ上部に表示する。
;; 2011-03-15
(which-function-mode 1)

;;;;; 全体のカスタマイズ
;;;タイトルバーにフルパスを表示
;; 130515
;; (setq frame-title-fomat "%f")
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 130515
(put 'narrow-to-region 'disabled nil)
;;; narrowing を禁止

;;文字コード自動判別無効
(setq auto-coding-functions nil)

;;; 行番号表示 -> nlinum.elへ移行
;; 121209
;; (global-linum-mode)

;;; yes no → y n
;; 121209
(fset 'yes-or-no-p 'y-or-n-p)

;;; 常時デバッグモード
;; 121209
(setq debug-on-error t)

;; 121207
;; タブを半角スペースに
;(setq-default tab-width 4 indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                      64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;;; ページ送り
;; ;; 1行ずつページ送りする
;; (setq scroll-conservatively 1)
;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;;; 説明文等の表示
;; 121207
;; bzrやsvn管理下のファイルをシンボリックリンク経由で開くとき確認をとらない
(setq vc-follow-symlinks t)
;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)

;; M-TABのキーバインドを変更しない
;; 2011-03-27
(setq flyspell-use-meta-tab nil)
;; デフォルトで自動スペルチェック機能を有効にする
(setq-default flyspell-mode t)
;; スペルチェックには英語の辞書を使う
(setq ispell-dictionary "american")

;;; バッファ自動再読み込み
;; 130502
(global-auto-revert-mode 1)

;;; window関連
;; 131109
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))
