;;; my-bench-startup.el --- 起動コストの内訳を計測する  -*- lexical-binding: t; -*-

;; issue #7（elpaca 移行の検討）の判断材料を得るための計測プローブ。
;;
;; 目的は「非同期パッケージ管理へ移行した場合に削減できる時間の上限」を求めることである。
;; その上限は、現行の同期構成で外部パッケージの活性化に費やしている時間を超えない。
;; したがって elpaca を導入せずに、現行構成の内訳を測るだけで上限が得られる。
;;
;; 実 pty（emacs -nw）で起動し、遅延ロードの完了（READY）まで待ってから
;; use-package の統計を集計し、機械可読な行を標準エラーへ出力して終了する。
;;
;; 前提: 呼び出し側の shim が early-init より前に use-package-compute-statistics を t にする。

;;;;; [Group] Bench - 依存 ;;;;;
(require 'my-test-deferred)

(unless (boundp 'my-test-deferred--feature-modes)
  (error "my-test-deferred--feature-modes が未定義です"))

;;;;; [Group] Bench - READY 述語 ;;;;;
;; 判定条件は tests/my-test-tty-live.el の my-test-tty-live--deferred-ready-p と同一に保つ。
;; 独自の READY 定義を増やさない（条件がずれると計測値の意味がずれるため）。
(defun my-bench--xclip-expected-p ()
  "xclip が有効であるべき環境なら non-nil を返す。"
  (and (not (display-graphic-p))
       (getenv "DISPLAY")
       (executable-find "xclip")
       t))

(defun my-bench--ready-p ()
  "遅延ロード対象と条件付き xclip が有効なら non-nil を返す。"
  (let ((ready t))
    (dolist (entry my-test-deferred--feature-modes)
      (unless (and (featurep (car entry))
                   (boundp (cdr entry))
                   (default-value (cdr entry)))
        (setq ready nil)))
    (and ready
         (or (not (my-bench--xclip-expected-p))
             (and (featurep 'xclip)
                  (bound-and-true-p xclip-mode))))))

;;;;; [Group] Bench - パッケージ分類 ;;;;;
(defun my-bench--package-class (package)
  "PACKAGE が外部管理か組み込みかを返す。

解決先が straight のビルドディレクトリ配下なら `external'、
それ以外で解決できれば `builtin'、解決できなければ `unknown' を返す。
`:straight nil' でも親パッケージのビルド配下に同梱される拡張
（vertico-repeat 等）は external として数える。非同期化で遅延しうる
コストはビルド配下にあるかどうかで決まるためである。

ディレクトリ名は公開変数 `straight-build-dir' から取る。既定値 \"build\" を
文字列で仮定すると、`straight-use-version-specific-build-dir' 有効時に
すべて builtin へ化ける。"
  (let* ((name (symbol-name package))
         (lib (ignore-errors (locate-library name))))
    (cond
     ((null lib) 'unknown)
     ((and (fboundp 'my-set-straight)
           (bound-and-true-p straight-build-dir)
           (ignore-errors
             (file-in-directory-p lib (my-set-straight straight-build-dir "/"))))
      'external)
     (t 'builtin))))

;;;;; [Group] Bench - 集計と出力 ;;;;;
(defun my-bench--emit (format-string &rest args)
  "計測結果を機械可読な 1 行として標準エラーへ出す。"
  (princ (concat "MY_BENCH " (apply #'format format-string args) "\n")
         #'external-debugging-output))

(defun my-bench--elapsed-since-init ()
  "プロセス起動からの経過秒を返す。"
  (float-time (time-subtract (current-time) before-init-time)))

;; 計測区間が他の宣言を内包してしまう「容器」宣言。合計から除外する。
;;
;; init.el:25-31 は (init-loader-load ...) を use-package init-loader の :config 内で
;; 呼ぶため、init-loader の :use-package-secs は loads/inits/ の全宣言を内包する。
;; これを加算すると全体が二重計上になる（実測で eager の総和が emacs-init-time を
;; 超えることで検出した）。
(defconst my-bench--container-packages '(init-loader)
  "計測区間が他宣言を内包するため合計から除外する宣言。")

(defun my-bench--secs (stats keyword)
  "STATS から KEYWORD の計測秒を取り出す。未計測なら 0.0 を返す。"
  (let ((v (gethash keyword stats)))
    (if v (float-time v) 0.0)))

;; `use-package-statistics-time' は :use-package / :preface / :init / :config の
;; 4 区間を単純加算するが、use-package-core.el の gather 呼び出し位置
;; (:use-package が 1916/1928、:preface 1292/1297、:init 1640/1652、:config 1677/1689)
;; から分かるとおり :use-package は他 3 つを内包する最外周区間である。
;; したがって単純加算は二重・三重計上になる（実測でも合計が経過時間を超えた）。
;; ここでは宣言 1 件あたりの実コストとして :use-package-secs だけを使う。
;;
;; なお :defer 付き宣言の :config は use-package の区間より後（初回描画の後）に
;; 走るため :use-package-secs には含まれない。これは意図どおりで、
;; 「初回描画より前に払っているコスト」を切り出す本計測の目的に合致する。
(defun my-bench--totals ()
  "分類ごとの (eager 秒 deferred-config 秒 件数) を alist で返す。"
  (let ((totals (list (cons 'external (list 0.0 0.0 0))
                      (cons 'builtin (list 0.0 0.0 0))
                      (cons 'unknown (list 0.0 0.0 0)))))
    (when (hash-table-p (bound-and-true-p use-package-statistics))
      (maphash
       (lambda (package stats)
         (unless (memq package my-bench--container-packages)
           (let* ((class (my-bench--package-class package))
                  (eager (my-bench--secs stats :use-package-secs))
                  (conf (my-bench--secs stats :config-secs))
                  (cell (alist-get class totals)))
             (setf (nth 0 cell) (+ (nth 0 cell) eager)
                   (nth 1 cell) (+ (nth 1 cell) conf)
                   (nth 2 cell) (1+ (nth 2 cell))))))
       use-package-statistics))
    totals))

(defun my-bench--report-statistics ()
  "use-package の統計を分類ごとに集計して出力する。"
  (if (not (and (bound-and-true-p use-package-compute-statistics)
                (hash-table-p (bound-and-true-p use-package-statistics))))
      (my-bench--emit "error=use-package-statistics-unavailable")
    (maphash
     (lambda (package stats)
       (my-bench--emit "pkg name=%s class=%s eager=%.6f config=%.6f"
                       package
                       (my-bench--package-class package)
                       (my-bench--secs stats :use-package-secs)
                       (my-bench--secs stats :config-secs)))
     use-package-statistics)
    (let ((totals (my-bench--totals)))
      (dolist (class '(external builtin unknown))
        (let ((cell (alist-get class totals)))
          (my-bench--emit "total class=%s count=%d eager=%.6f config=%.6f"
                          class (nth 2 cell) (nth 0 cell) (nth 1 cell)))))))

;;;;; [Group] Bench - ランナー ;;;;;
(defvar my-bench--t1 nil
  "window-setup-hook 到達時点の経過秒。")

(defvar my-bench--totals-at-t1 nil
  "window-setup-hook 到達時点の分類別集計。")

(defun my-bench--run (&optional from-idle-timer)
  "READY まで待って計測結果を出力し、Emacs を終了する。"
  (if (not from-idle-timer)
      ;; window-setup-hook のスタック中は :defer の idle timer が進まないため、
      ;; 最初に idle になった時点へ同じランナーを引き渡す
      ;; （tests/my-test-tty-live.el と同じ理由・同じ構造）。
      (progn
        (setq my-bench--t1 (my-bench--elapsed-since-init))
        ;; 初回描画の時点で「すでに払い終えたコスト」を切り出す。
        ;; 非同期化が短縮しうるのはこの区間だけであり、これより後に
        ;; :defer で流れている分は現行構成でも既に描画の後ろにある。
        (setq my-bench--totals-at-t1 (my-bench--totals))
        (run-with-idle-timer 0 nil #'my-bench--run t))
    (condition-case err
        (let ((attempts 0))
          (while (and (< attempts 300)
                      (not (my-bench--ready-p)))
            (setq attempts (1+ attempts))
            (sit-for 0.1))
          (my-bench--emit "t1_window_setup=%.6f" (or my-bench--t1 -1.0))
          (my-bench--emit "t3_ready=%.6f" (my-bench--elapsed-since-init))
          (my-bench--emit "ready=%s attempts=%d"
                          (if (my-bench--ready-p) "yes" "no")
                          attempts)
          (my-bench--emit "emacs_init_time=%s" (emacs-init-time "%.6f"))
          ;; 初回描画時点のスナップショット（= 非同期化が短縮しうる上限）
          (let ((snapshot (or my-bench--totals-at-t1 (my-bench--totals)))
                (eager-sum 0.0))
            (dolist (class '(external builtin unknown))
              (let ((cell (alist-get class snapshot)))
                (setq eager-sum (+ eager-sum (nth 0 cell)))
                (my-bench--emit "at_t1 class=%s count=%d eager=%.6f config=%.6f"
                                class (nth 2 cell) (nth 0 cell) (nth 1 cell))))
            ;; 健全性検査: 宣言評価はすべて初回描画より前に終わるため、
            ;; eager の総和は t1 を超えてはならない。超えたら計上方法が誤っている。
            (my-bench--emit "sanity eager_sum=%.6f t1=%.6f ok=%s"
                            eager-sum (or my-bench--t1 -1.0)
                            (if (and my-bench--t1 (<= eager-sum my-bench--t1))
                                "yes" "no")))
          (my-bench--report-statistics)
          (my-bench--emit "end")
          (kill-emacs (if (my-bench--ready-p) 0 1)))
      (error
       (my-bench--emit "error=%S" err)
       (kill-emacs 1)))))

(add-hook 'window-setup-hook #'my-bench--run 90)

(provide 'my-bench-startup)
;;; my-bench-startup.el ends here
