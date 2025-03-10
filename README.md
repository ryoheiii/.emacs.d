<!-- -*- gfm -*- -->
# Emacs 環境構築ガイド

## 1. Emacs のセットアップ

### 1.1 依存パッケージのインストール
Emacs のビルドに必要なパッケージをインストールするには、以下のスクリプトを実行してください。

``` sh
./emacs-setup.sh --setup
```

### 1.2 インストール可能なバージョンの確認
利用可能な Emacs のバージョンを確認するには、以下のコマンドを実行してください。

``` sh
./emacs-setup.sh --list
```

### 1.3 Emacs のインストール
特定のバージョンの Emacs をビルド・インストールするには、以下のコマンドを実行します。

``` sh
./emacs-setup.sh --install <バージョン>
```

例: Emacs 30.1 をインストールする場合
``` sh
./emacs-setup.sh --install 30.1
```

### 1.4 Emacs のアンインストール
インストールした Emacs を削除するには、以下のコマンドを実行します。

``` sh
./emacs-setup.sh --uninstall
```

### 1.5 Emacs の自動生成ファイルの削除
Emacs 起動後のキャッシュや一時ファイルを削除するには、以下のコマンドを実行します。

``` sh
./emacs-setup.sh --clean
```

---

## 2. Emacs 設定ファイルのパス設定
以下のパスを設定してください。

### 設定する項目
- **Migemo**: 辞書のインストール場所を `loads/inits/20-external-package.el` で設定

---

## 3. 初回起動後の追加設定
Emacs の初回起動後、C/C++ ソースコードを開いて以下のコマンドを実行してください。

### 設定項目
- **Irony サーバーの構築**: Emacs 起動後、C/C++ ソースコードを開いて以下のコマンドを実行
``` sh
 `M-x irony-install-server
```

---

## 4. パッケージ管理
Emacs のパッケージは以下のディレクトリで管理します。

- `~/.emacs.d/loads/elisp`
- `~/.emacs.d/loads/site-elisp`

---

## 5. 設定ファイルの命名規則
Emacs の設定ファイル (`~/.emacs.d/loads/inits/*.el`) には、以下の命名規則を適用してください。

### 命名規則
- **環境に依存しない設定ファイル**
  - ファイル名の先頭に 2 桁の数字を付与 (例: `00-keybind.el`)
  - 数字は読み込み順を示し、`00` が最初、`99` が最後
  - 同じ数字を付けることも可能
- **環境依存の設定ファイル**
  - 環境ごとに以下のプレフィックスを使用:
    - Meadow: `meadow-`
    - Carbon Emacs: `carbon-emacs-`
    - Cocoa Emacs: `cocoa-emacs-`
    - Terminal (`emacs -nw`): `nw-`
    - Windows: `windows-`
    - Linux: `linux-`
  - 例: `cocoa-emacs-appearance.el` (`<環境>-<機能名>.el`)
- **package.el 関連の設定は 20 番台に記述**

---

## 6. その他の注意点
- ターミナルで Emacs を利用する際は、`xterm-256color` を設定
