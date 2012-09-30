# emacs-gdev
Emacs上でのGauche開発支援を目的にしたライブラリです。  
Vim用のvim-gdevをEmacsに移植したものなので、 

- gdev.el
- goshcomp.el
- gosh-anything.el

以外のファイルはすべてvim-gdevと同じものを利用しています。

### 依存するスクリプト
必須:  
[json.el](http://edward.oconnor.cx/2006/03/json.el)  
あった方がいいもの:  
[Auto Complete Mode](http://cx4a.org/software/auto-complete/index.ja.html)  
[Anything](http://www.emacswiki.org/Anything)  

### 依存する外部プログラム
Gauche 0.9.2以上

## 機能
1. ソースコード内の大域定義とuseしているモジュールの解析を行い 補完候補としてリストアップ
2. ginfoコマンドによってシンボルの情報を表示
    - 表示する内容
        - シンボルタイプ(変数、関数、クラス...)
        - 定義モジュール(ファイル)名
        - シンボル名
        - 関数であれば引数、クラスであればスロット名
        - それぞれに付随するドキュメント
    - 表示内容の例(lambdaを選択した場合)

            Function  ---null
            (lambda formals body ...)

            --description--
            [R5RS+]
            この式は評価されると手続きを生成します。この式が評価された時点の環境が手続き中に保持されます。
            手続きが呼ばれると、記憶された環境に引数の束縛を追加した環境中でbody が順に評価され、
            最後の式の値が返されます。
            ...
3. anythingを利用したginfo検索
    - anything-ginfo-sourceを実装。  
    ginfo-anything、ginfo-with-word-anythingコマンドによって、anything-ginfo-sourceを呼び出してginfoをanythingのインタフェースを利用して実行できます。  
    ginfo-with-word-anythingコマンドは現在のカーソル位置のシンボルを入力としてanythingを実行し、 候補がない一つしかない場合はすぐにinfo画面を起動するのでキーマップに割り当てて使用することをお勧めします。
4. ロード可能なモジュール全てから横断的に検索
    - anything-ginfo-all-symbol-sourceを実装。  
    動的にロードパスから全てのモジュールを探し出しシンボルを一覧できます。  
    anything起動後も非同期で候補の追加と更新を行うため目的のシンボルがなかなか現れないとうことが起こる可能性があります。
5. モジュール一覧検索 -> モジュール内のシンボル一覧検索
    - ginfo-all-moduleコマンドを実装。  
    ロード可能な全てのモジュールを一覧から選択、次にモジュール内のシンボルを一覧から選択しginfoを表示します。  
    anything-ginfo-all-module-sourceと対応するginfo-anything-all-moduleコマンドを実装。
6. 定義へジャンプ
    - gdev:gosh-goto-define関数を実装。  
    シンボルが定義されたファイルを開きその行へジャンプします。  
    schemeレベルで定義されているシンボルであればGaucheライブラリ内で定義されたシンボルにもジャンプすることができます。  
    ただし、ジャンプ前の地点を保存していないので元の場所には戻れません。  
    Basic setupで設定しているように適当なキーにマッピングして使用してください。


## Basic setup

    (require 'gdev)
    ;;set install directory
    (setq gdev:root-dir "~/.emacs.d/emacs-gdev")
    
    ;;auto-completeがインストールされいる場合
    (require 'goshcomp)
    ;;anythingがインストールされている場合
    (require 'gosh-anything)
    
    (defun scheme-mode-hooks ()
      ;;auto-completeにGauche用のソースを追加
      (setq ac-sources (append goshcomp:all-source ac-sources))

      ;;ginfo-with-word-anythingコマンドをC-cgkに割り当て
      (define-key scheme-mode-map "\C-cgk" 'ginfo-with-word-anything)
      ;;ginfoバッファのクローズ
      (define-key scheme-mode-map "\C-c0" 'gdev:close-ginfo)
      ;;ginfoバッファのスクロールアップ・ダウン
      (define-key scheme-mode-map "\C-c\C-n" 'gdev:scroll-up-ginfo)
      (define-key scheme-mode-map "\C-c\C-p" 'gdev:scroll-down-ginfo)

      ;;現在のバッファでカーソル位置のシンボルが定義されている場所にジャンプ
      (define-key scheme-mode-map [f12] (lambda ()
                                          (interactive)
                                          (gdev:gosh-goto-define (symbol-name (symbol-at-point)) nil)))
      ;;横方向に分割したバッファでカーソル位置のシンボルが定義されている場所にジャンプ
      (define-key scheme-mode-map [f11] (lambda ()
                                          (interactive)
                                          (gdev:gosh-goto-define (symbol-name (symbol-at-point)) 'h)))
      ;;縦方向に分割したバッファでカーソル位置のシンボルが定義されている場所にジャンプ
      (define-key scheme-mode-map [f10] (lambda ()
                                          (interactive)
                                          (gdev:gosh-goto-define (symbol-name (symbol-at-point)) 'v)))
      )
    ;;フックをschemeモードに登録
    (add-hook 'scheme-mode-hook 'scheme-mode-hooks)
