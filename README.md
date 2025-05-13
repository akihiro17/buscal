# Buscal

**Buscal is a personal toy project and is not intended for use in production environments.**

Buscalは、[Rustで作るプログラミング言語 | 技術評論社](https://gihyo.jp/book/2024/978-4-297-14192-9)を参考にして作ったBashスクリプトにコンパイルされるプログラミング言語。

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
# 変数の定義
var greeting: str = "こんにちは";
var name: str = "世界";

echo(format("{}, {}!", greeting, name));
```

**生成されるBashスクリプト:**
```bash
#!/usr/bin/env bash
set -e
# 変数の定義
greeting="こんにちは"
name="世界"

echo "${greeting}, ${name}!"

```

### 配列とループ

**array_example.bscl:**
```
# 配列の定義
var fruits: array::<str> = ["りんご", "みかん", "ばなな"];
var count: i64 = len(fruits);

echo(format("果物の数: {}", count));

# ループ
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

# ループ
for i in $(seq 0 $(("${count}" - 1))); do
  echo "$(("${i}" + 1)). ${fruits["${i}"]}"
done
```

### コマンド実行と条件分岐

**command.bscl**
```
# コマンド実行
if execute(["ls", "-al"]) {
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
if "ls" "-al"; then
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

## ライセンス

MIT License
