# reversi-AI

## What I implemented

- NegaScout
- 反復深化法
- 相手の着手可能数による move ordering
- OCaml の Int64 型を用いたビットボード
- 評価関数 (確定石の数・着手可能数などによる)
- 反復深化の時間制限など対戦で負けた際のパラメータの調整

## Strategy

### Early

[logistelloのopeningbook](https://skatgame.net/mburo/log.html) を読み込んで定石としている．

### Middle

評価関数の値が最も良い手を選ぶ．
読む手数は時間制限が許す限り反復深化法で増やす．

### Final

残りの手数が22手になったら読み切る．
