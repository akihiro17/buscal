# BusCal

[Rustで作るプログラミング言語 | 技術評論社](https://gihyo.jp/book/2024/978-4-297-14192-9)

BusCalは、Bashスクリプトにコンパイルされるドメイン固有言語（DSL）です。Rustで実装されており、型チェック機能付きのカスタムスクリプト言語を提供します。

## 使用方法

### BusCalスクリプトの実行

```bash
# 直接実行
cargo run run script.bscl

# Bashスクリプトにコンパイル
cargo run build script.bscl -o output.sh

# 生成されたBashスクリプトを実行
./output.sh
```

## サンプルコード

### 基本的なHello World

**hello.bscl:**
```
# 変数の定義（型注釈付き）
var greeting: str = "こんにちは";
var name: str = "世界";

# 文字列補間
echo(format("{}, {}!", greeting, name));
```

**生成されるBashスクリプト:**
```bash
#!/usr/bin/env bash
set -e
echo "こんにちは, 世界!"
```

### 配列とループ

**array_example.bscl:**
```
# 配列の定義
var fruits: array::<str> = ["りんご", "みかん", "ばなな"];
var count: i64 = len(ref fruits);

echo(format("果物の数: {}", count));

# ループ処理
for i in 0 to count - 1 {
    echo(format("{}. {}", i + 1, fruits[i]));
}
```

**生成されるBashスクリプト:**
```bash
#!/usr/bin/env bash
set -e
# 配列の定義
fruits=("りんご" "みかん" "ばなな")
count=${#fruits[@]}

echo "果物の数: ${count}"

# ループ処理
for i in $(seq 0 $(("${count}" - 1))); do
  echo "$(("${i}" + 1)). ${fruits["${i}"]}"
done

```

### コマンド実行と条件分岐

**command.bscl**
```
# コマンド実行
var cmd: array::<str> = ["ls", "-la"];
if execute(cmd) {
    echo("ディレクトリ一覧を表示しました");
} else {
    echo("コマンドの実行に失敗しました");
}
```

**生成されるBashスクリプト:**
```bash
#!/usr/bin/env bash
set -e
# コマンド実行
cmd=("ls" "-la")
if "${cmd[@]}"; then
  echo "ディレクトリ一覧を表示しました"
else
  echo "コマンドの実行に失敗しました"
fi
```

## 言語仕様

### データ型

- `str`: 文字列型
- `i64`: 整数型
- `array::<str>`: 配列型(文字列型)
- `array::<i64>`: 配列型(整数型)

### 組み込み関数

- `echo(message)`: メッセージを出力
- `format(template, args...)`: 文字列フォーマット（`{}`でプレースホルダ）
- `execute(command_array)`: コマンド配列を実行

### 制御構文

- `if/else`: 条件分岐
- `for i in start to end`: 範囲ループ
- `break/continue`: ループ制御

## 開発

### テストの実行

```bash
# 全テストを実行
make testall

# 個別テストを実行
make test TARGET=test/echo.bscl

# Rustのユニットテストを実行
cargo test
```

### 開発コマンド

```bash
# フォーマット
cargo fmt

# 警告を無視して実行
RUSTFLAGS=-Awarnings cargo run run script.bscl
```

## ライセンス

MIT License
