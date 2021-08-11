# Generator
_Intermediate code generator_


## 中間命令列生成器

意味解析を行った後に得られる Semantic graph から中間命令列を生成する．




## プログラム構成

- [instruction.ml](instruction.ml)
  - Definitions for intermediate instructions

- [register_table.ml](register_table.ml)
  - Associate each links to the assigned registers

- [match.ml](match.ml)
  - Generate instructions for the graph pattern matching

- [pushout.ml](pushout.ml)
  - Generate instructions for the pushout of the graph

- [generator.ml](generator.ml)
  - The toplevel of the intermediate code generator


