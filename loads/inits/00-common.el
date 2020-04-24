;; 日本語設定
(set-language-environment "Japanese")
;; (set-locale-environment nil)

;;;文字コードの設定
(set-language-environment   'Japanese)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 行末の空白をファイルセーブ時に削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Macで日本語のファイル名を扱う場合の設定
(when (eq system-type 'darwin)
  (use-package ucs-normalize
    :config
    (set-file-name-coding-system 'utf-8-hfs)
    (setq locale-coding-system 'utf-8-hfs)
    ))

;; 行末の空白をファイルセーブ時に削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; 色設定関連
;;; theme
;;; Please set your themes directory to 'custom-theme-load-path (replace-colorthemes)
;; Emacs 24 以上ならば color-theme ではなく theme フレームワークが利用可能なのでコメントアウト
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/loads/site-elisp/replace-colorthemes/"))

;; load your favorite theme (replace-colorthemese)
;; https://github.com/emacs-jp/replace-colorthemes/blob/master/screenshots.md
(load-theme 'hober t t)
(enable-theme 'hober)
;; (load-theme 'railscast t t)
;; (enable-theme 'railscast)
;; (load-theme 'clarity t t)
;; (enable-theme 'clarity)
;; (load-theme 'dark-laptop t t)
;; (enable-theme 'dark-laptop)

;; ソースコードに色を付ける
(global-font-lock-mode t)
(transient-mark-mode t)

;;; バー
;; メニューバーを消す
(menu-bar-mode -1)
;; ツールバーを消す
(if window-system
    (tool-bar-mode 0))

;;; カーソル
;; カーソルの点滅を止める
(blink-cursor-mode 0)

;;; eval
;; evalした結果を全部表示
(setq eval-expression-print-length nil)

;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;; カーソルの位置が何行目かを表示する
(line-number-mode t)


;;; 行
;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)
;; 最終行に必ず一行挿入しない
(setq require-final-newline nil)

;;; バックアップ
;; バックアップファイルを作らない
(setq backup-inhibited t)
;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 自動保存ファイルを削除する
(setq delete-auto-save-files t)

;; ロックファイルを作成しない
(setq create-lockfiles nil)

;; 改行コードを表示する
(setq eol-mnemonic-dos  "(CRLF)")
(setq eol-mnemonic-mac  "(CR)")
(setq eol-mnemonic-unix "(LF)")

;;; 補完
;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; 部分一致の補完機能を使う
;; p-bでprint-bufferとか
;; Emacs 24ではデフォルトで有効になっていて、`partial-completion-mode'は
;; なくなっている。カスタマイズする場合は以下の変数を変更する。
;;   * `completion-styles'
;;   * `completion-pcm-complete-word-inserts-delimiters'
(if (fboundp 'partial-completion-mode)
    (partial-completion-mode t))
;; 補完可能なものを随時表示
;; 少しうるさい
(icomplete-mode 1)

;; クリップボードをシステムと共有する
(setq x-select-enable-clipboard t)

;;; 履歴
;; 履歴数を大きくする
(setq history-length 3000)
;; ミニバッファの履歴を保存する
(savehist-mode 1)
;; 最近使ったファイルの表示数
(setq recentf-max-menu-items 10)
;; 最近開いたファイルを保存する数を増やす
(setq recentf-max-saved-items 3000)

;;; shebangがあるファイルを保存すると実行権をつける。
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; リージョンの大文字小文字変換を有効にする。
;; C-x C-u -> upcase
;; C-x C-l -> downcase
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; 現在の関数名をウィンドウ上部に表示する。
(which-function-mode 1)

;;;;; 全体のカスタマイズ
;;;タイトルバーにフルパスを表示
;; (setq frame-title-fomat "%f")
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

(put 'narrow-to-region 'disabled nil)
;;; narrowing を禁止

;;; 行番号表示 -> nlinum.elへ移行
 (global-linum-mode)
;; 4 桁分の表示領域を確保する
(setq linum-format "%4d ")
;; 軽くする処理
(setq linum-delay t)

;;; yes no → y n
(fset 'yes-or-no-p 'y-or-n-p)

;;; 常時デバッグモード
;; (setq debug-on-error t)

;; タブを半角スペースに
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                      64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
;;文字コード自動判別無効
(setq auto-coding-functions nil)

;;   ### diffを見やすく ###
;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)

;;; ページ送り
;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;;; 説明文等の表示
;; bzrやsvn管理下のファイルをシンボリックリンク経由で開くとき確認をとらない
(setq vc-follow-symlinks t)
;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)

;; M-TABのキーバインドを変更しない
(setq flyspell-use-meta-tab nil)

;;; バッファ自動再読み込み
(global-auto-revert-mode 1)
