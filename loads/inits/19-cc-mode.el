;;; cc-mode
;; C言語と同じような構文のプログラミング言語用の設定
;; 2012-03-18
;; ref: http://tubo028.hatenablog.jp/entry/2013/06/09/011042

;; c-modeやc++-modeなどcc-modeベースのモード共通の設定
(add-hook
 'c-mode-common-hook
 (lambda ()
   ;; コンパイル
   (local-set-key (kbd "C-c c") 'compile)

   ;; BSDスタイルをベースにする
   (c-set-style "bsd")

   (setq c-indent-level 4)

   ;; スペースでインデントをする
   (setq indent-tabs-mode nil)

   ;; インデント幅を4にする
   (setq tab-width 4)
   (setq c-basic-offset 4)

   ;; 自動改行（auto-new-line）と
   ;; 連続する空白の一括削除（hungry-delete）を
   ;; 有効にする
   (c-toggle-auto-hungry-state 1)
   ;; (c-toggle-hungry-state 1)

   ;; 定義開始時の開き波かっこ「{」の前で改行しない
   ;; デフォルトでは前後に改行が入る
   ;; afterを指定すると後だけに改行がはいる
   ;; (defun-open before after)にすると前後に改行が入る
   (setq c-hanging-braces-alist
         `((defun-open after)   ; トップレベルの関数定義
           ,@c-hanging-braces-alist))

   ;; defineマクロ
   ;; #define ***
   (c-set-offset 'cpp-macro 0)
   (c-set-offset 'cpp-macro-cont '+)

   ;; extern "C" { *** }
   (c-set-offset 'extern-lang-open 0)
   (c-set-offset 'inextern-lang '+)
   (c-set-offset 'extern-lang-close 0)

   ;; ラベル
   ;; *** :
   (c-set-offset 'label 0)

   ;; 普通の文
   ;; *** ;
   (c-set-offset 'statement 0)

   ;; 式の途中で改行した時
   ;; hoge =
   ;;   *** ;
   (c-set-offset 'statement-cont '+)

   ;; 複数行に及ぶ文字列リテラル
   ;; string "aaaaa
   ;; ***"
   (c-set-offset 'string 0)

   ;; テンプレート引数
   ;; template<typeneme T ,
   ;;   ***>
   (c-set-offset 'template-args-cont '+)

   ;; throw, const など，引数リストと中身の間
   ;; int main() *** {}
   (c-set-offset 'func-decl-cont '+)

   ;; }で終わる式
   ;; enum {***}; etc.
   (c-set-offset 'block-open 0)
   (c-set-offset 'statement-block-intro '+)
   (c-set-offset 'block-close 0)

   ;; コメント
   ;; // ***
   (c-set-offset 'comment-intro 0)
   ;; /* *** */
   (c-set-offset 'c 1)

   ;; case文のラベルと各場合の処理
   ;; case *** : ***
   (c-set-offset 'case-label 0)
   (c-set-offset 'statement-case-open '+)
   (c-set-offset 'statement-case-intro '+)


   ;; CamelCaseの語でも単語単位に分解して編集する
   ;; GtkWindow         => Gtk Window
   ;; EmacsFrameClass   => Emacs Frame Class
   ;; NSGraphicsContext => NS Graphics Context
   (subword-mode 1)

   ;; 他のエディタなどがファイルを書き換えたらすぐにそれを反映する
   ;; auto-revert-modeを有効にする
   (auto-revert-mode)))

;; c++-modeだけの設定
;; 2013-01-05
(add-hook
 'c++-mode-hook
 (lambda ()
   ;; 20-flycheck.elに定義
   ;; c++11オプションのために追加
   ;; todo: 150925: 研究の際，インクルードパスを通してないとエラーが出まくってうざいので，とりあえず無効に
;;;;   ;; (flycheck-select-checker 'c/c++)

   ;; 複数行にまたがるストリーム
   ;; cin
   ;;   << **
   (c-set-offset 'stream-op 0)

   ;; 定義開始時の開き波かっこ「{」の前で改行しない
   ;; デフォルトでは前後に改行が入る
   ;; afterを指定すると後だけに改行がはいる
   ;; (defun-open before after)にすると前後に改行が入る
   (setq c-hanging-braces-alist
         `((defun-open after)   ; トップレベルの関数定義
           (class-open after)   ; クラス定義
           (inline-open after)  ; クラス定義内のインラインメソッド定義
           ,@c-hanging-braces-alist))))
