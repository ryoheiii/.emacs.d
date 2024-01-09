;;; 文字コードの設定
(set-language-environment   "Japanese")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; Macで日本語のファイル名を扱う場合用
(when (eq system-type 'darwin)
  (use-package ucs-normalize
    :config
    (set-file-name-coding-system 'utf-8-hfs)
    (setq locale-coding-system 'utf-8-hfs)
    )
  )

;;; 改行コードの表示
(setq eol-mnemonic-dos  "(CRLF)")
(setq eol-mnemonic-mac  "(CR)")
(setq eol-mnemonic-unix "(LF)")

;;; ファイル保存時の設定
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; 行末の空白を削除

;;; ファイルの自動更新設定。ファイルの変更を自動的に反映
(global-auto-revert-mode 1)
(setq magit-auto-revert-mode t)

;;; 表示設定
(global-font-lock-mode t) ;; ソースコードを色付け
(transient-mark-mode t)
;; 日時表示
(setq display-time-day-and-date t)
(setq display-time-string-forms
      '((format "%s/%s (%s) %s:%s"
                month day dayname
                24-hours minutes
                ))
      )
(display-time)

;;; インターフェースの設定
(menu-bar-mode -1)                    ;; メニューバーを消す
(if window-system (tool-bar-mode 01)) ;; ツールバーを消す

;;; カーソル設定
(blink-cursor-mode 0) ;; カーソルの点滅を止める

;;; 評価設定
(setq eval-expression-print-length nil) ;; evalした結果を全部表示
(column-number-mode t)  ;; カーソル位置の列番号表示
(line-number-mode t)    ;; カーソル位置の行番号表示

;;; システム設定
(setq ring-bell-function 'ignore) ;; エラー音を鳴らさない
;; save時にmode line を光らせる
(add-hook 'after-save-hook
      (lambda ()
        (let ((orig-fg (face-background 'mode-line)))
          (set-face-background 'mode-line "dark green")
          (run-with-idle-timer 0.1 nil
                   (lambda (fg) (set-face-background 'mode-line fg))
                   orig-fg))))

;;; 行設定
(setq kill-whole-line t)         ;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq require-final-newline nil) ;; 最終行に必ず一行挿入しない

;;; バックアップ設定
(setq backup-inhibited t)       ;; バックアップファイルを作らない
(setq delete-auto-save-files t) ;; 終了時にオートセーブファイルを削除
(setq make-backup-files nil)    ;; バックアップを作成しない
(setq auto-save-default nil)    ;; オートセーブを無効にする
(setq create-lockfiles nil)     ;; ロックファイルを作成しない

;;; 補完設定
(setq completion-ignore-case t) ;; 補完時に大文字小文字を区別しない
(setq read-file-name-completion-ignore-case t)
(icomplete-mode 1)              ;; 補完可能なものを随時表示

;;; クリップボード設定
(setq x-select-enable-clipboard t) ;; クリップボードをシステムと共有

;;; 履歴設定
(setq history-length 3000)          ;; 履歴数を大きくする
(savehist-mode 1)                   ;; ミニバッファの履歴を保存する
(setq recentf-max-menu-items 10)    ;; 最近使ったファイルの表示数
(setq recentf-max-saved-items 3000) ;; 最近開いたファイルを保存する数を増やす

;;; シェルスクリプト（shebangがあるファイル）の保存時に実行権を付与
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; リージョンの大文字小文字変換を有効か
;; C-x C-u -> upcase
;; C-x C-l -> downcase
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; 現在の関数名をウィンドウ上部に表示
(which-function-mode 1)

;;; タイトルバーにフルパス表示
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;;; narrowing を禁止
(put 'narrow-to-region 'disabled nil)

;;; 行番号表示
(if (version< emacs-version "29")
    (global-linum-mode t)               ; Emacs28 以前
  (global-display-line-numbers-mode t)) ; Emacs29 以降
(setq linum-format "%4d ") ;; 4 桁分の表示領域を確保する
(setq linum-delay t)       ;; 軽くする処理

;;; 確認ダイアログを簡略化 (yes no → y n)
(fset 'yes-or-no-p 'y-or-n-p)

;;; タブの設定
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                        64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;;; diff 設定
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-split-window-function 'split-window-horizontally) ;; diff のバッファを上下ではなく左右に並べる

;;; スクロール設定
;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;;; その他の設定
(setq vc-follow-symlinks t)      ;; bzrやsvn管理下のファイルをシンボリックリンク経由で開くとき確認をとらない
(setq inhibit-startup-message t) ;; 起動時のメッセージを表示しない
(setq flyspell-use-meta-tab nil) ;; M-TABのキーバインドを変更しない
(global-auto-revert-mode 1)      ;; バッファ自動再読み込み
