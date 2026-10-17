;;; 00-core.el --- 基本設定  -*- lexical-binding: t; -*-
;;; Commentary:
;; 文字コード、保存時の処理、スクロール、起動後の GC を設定する。

;;; Code:

;; 遅延ロード先とプラットフォーム固有定義をコンパイラへ伝える。
(defvar w32-get-true-file-attributes)
(defvar w32-pipe-read-delay)
(defvar w32-pipe-buffer-size)
(defvar w32-use-native-image-API)

;;; 文字コード設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(cond
 (IS-MAC
  (when (coding-system-p 'utf-8-hfs)
    (set-file-name-coding-system 'utf-8-hfs)
    (setq locale-coding-system 'utf-8-hfs)))
 (IS-WINDOWS
  (setq-default default-process-coding-system '(utf-8-unix . japanese-cp932-dos))))

;;; ファイル操作関連
;; backup と auto-save は var/backup/ へ隔離して有効化(保存先は early-init.el で設定済み)
(setq make-backup-files t
      version-control t        ; 番号付きバックアップ
      delete-old-versions t    ; 古い版は確認なしで削除
      kept-new-versions 5
      kept-old-versions 1
      backup-by-copying t      ; コピーでバックアップし、元ファイルのリンク関係を保つ
      auto-save-default t
      delete-auto-save-files t
      create-lockfiles nil)    ; .#lock は作業ディレクトリを汚すため無効を維持

;; auto-save-visited-mode の idle 自動保存は save-some-buffers 経由で before-save-hook と
;; after-save-hook を走らせる。入力途中の末尾空白を消したり 30 秒ごとにモードラインを
;; 光らせたりしないよう、タイマー経由の保存を識別するフラグを置く。束縛するのは
;; 18-built-in-package.el の my/auto-save-visited-save-some-buffers。
(defvar my/auto-save-visited-in-progress nil
  "`auto-save-visited-mode' のタイマーによる保存中なら non-nil。")

;; 明示的な保存をモードラインの点滅で知らせる。
(defvar my/mode-line-flash-timer nil
  "mode-line フラッシュの復元用タイマー。進行中は non-nil。")
(defun my/flash-mode-line-on-save ()
  "保存時に mode-line を一瞬光らせる。フラッシュ進行中の再入では元色を再取得しない。
idle 自動保存では光らせない。"
  (unless (or my/auto-save-visited-in-progress (timerp my/mode-line-flash-timer))
    (let ((orig (face-background 'mode-line)))
      (set-face-background 'mode-line "dark green")
      (setq my/mode-line-flash-timer
            (run-with-idle-timer 0.1 nil
                                 (lambda ()
                                   (set-face-background 'mode-line orig)
                                   (setq my/mode-line-flash-timer nil)))))))
(add-hook 'after-save-hook #'my/flash-mode-line-on-save)

;; シェルスクリプト（shebangがあるファイル）の保存時に実行権を付与
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; 行末の空白を保存時に削除(markdown はハードブレイク用の末尾空白があるため除外)
(defun my/delete-trailing-whitespace-except-markdown ()
  "Markdown 系バッファ以外で行末の空白を削除する。
idle 自動保存では入力途中の末尾空白を消さないよう何もしない（明示保存で削除する）。"
  (unless (or my/auto-save-visited-in-progress (derived-mode-p 'markdown-mode))
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook #'my/delete-trailing-whitespace-except-markdown)

;;;; 全フレームのフォーカスアウト時に全バッファを保存して GC
;; after-focus-change-function は短時間の状態揺れを伴い反復呼び出しされ得る上、
;; read-event 内など任意のコンテキストから呼ばれる (同変数の docstring のとおり)。
;; 短時間に続く通知をアイドルタイマーでまとめ、全フレームがフォーカスを
;; 失ったと判定した最初の 1 回だけ保存・GC を実行する。
(defvar my/all-frames-unfocused-p nil
  "直前の判定時点で全フレームがフォーカスを失っていたなら non-nil.")
(defvar my/focus-change-timer nil
  "フォーカス変化のデバウンス用 idle timer.")
(defun my/handle-focus-change ()
  "安定後のフォーカス状態を確認し、全フレーム喪失への遷移時のみ保存・GC を行う."
  (setq my/focus-change-timer nil)
  (let ((unfocused (not (seq-some #'frame-focus-state (frame-list)))))
    (when (and unfocused (not my/all-frames-unfocused-p))
      (save-some-buffers "!")
      (garbage-collect))
    (setq my/all-frames-unfocused-p unfocused)))
(defun my/after-focus-change ()
  "フォーカス変化をデバウンスし、状態が安定してから my/handle-focus-change を実行する."
  (when (timerp my/focus-change-timer)
    (cancel-timer my/focus-change-timer))
  (setq my/focus-change-timer
        (run-with-idle-timer 0.2 nil #'my/handle-focus-change)))

(add-function :after after-focus-change-function #'my/after-focus-change)

;;; 補完設定
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;;; 編集コマンドと関数名表示
;; narrowing を有効化
(put 'narrow-to-region 'disabled nil)
;; `upcase-region` / `downcase-region` を有効化
(put 'upcase-region 'disabled nil)   ; C-x C-u -> upcase
(put 'downcase-region 'disabled nil) ; C-x C-l -> downcase
;; 現在の関数名をモードラインに表示する。
(which-function-mode 1)

;;; スクロール設定
(setq scroll-conservatively 101
      scroll-margin 0
      comint-scroll-show-maximum-output t)

;;; 起動後の GC と警告
;; early-init.el で most-positive-fixnum に設定した GC 閾値を起動完了後に復元
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.2)))
(add-to-list 'warning-suppress-types '(undo discard-info))

;;; 起動時の表示・確認と競合回避
(setq vc-follow-symlinks t
      inhibit-startup-message t
      flyspell-use-meta-tab nil
      native-comp-async-report-warnings-errors 'silent)

;;; プロセス通信と表示負荷
;; 外部プロセスの出力を 1 回に最大 1 MiB 読み取る。
;; 参考: https://ayatakesi.github.io/lispref/25.2/html/Output-from-Processes.html
(setq read-process-output-max (* 1024 1024))
;; 参考: https://protesilaos.com/emacs/dotemacs
(setq blink-matching-paren nil)         ; 閉じ括弧を入力しても点滅させない
;; 参考: https://github.com/doomemacs/doomemacs/blob/master/lisp/doom-start.el
;; モード判定で、大文字小文字を無視した拡張子の再検索をしない。
(setq auto-mode-case-fold nil)
;; 双方向テキストの括弧解析を省き、長い行の表示負荷を抑える。
(setq bidi-inhibit-bpa t)
;; フォーカスされていないウィンドウのカーソルを削除
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
;; フォントの厳密な計測を省いてスクロールを速くする。
(setq fast-but-imprecise-scrolling t)
;; ffap で未知のホスト名の到達確認をしない。
(setq ffap-machine-p-known 'reject)

;; `idle-update-delay' は Emacs 30.1 で obsolete のため後継変数を使用
(setq which-func-update-delay 1.0)
;; 入力待ちがある間は、再描画時の色付け処理を後回しにする。
(setq redisplay-skip-fontification-on-input t)
;;; OS 固有の最適化
;; Windows のファイル情報取得とプロセス通信を調整する。
(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)))
;; 参考: https://github.com/seagle0128/.emacs.d
(when IS-WINDOWS
  (setq w32-use-native-image-API t))
(unless IS-MAC
  (setq command-line-ns-option-alist nil))

(provide '00-core)
;;; 00-core.el ends here
