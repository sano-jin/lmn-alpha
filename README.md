# lmn-alpha
_Experimental_

A minimal compiler and runtime for a language based on graph rewriting.


## Getting Started
### Prerequisites
- [opam](https://opam.ocaml.org/)

### Installation
```bash
git clone https://github.com/sano-jin/lmn-alpha
cd lmn-alpha
opam install .
dune build
```

## Usage

```bash
./run example/append.lmn -t
```


If [the program (example/append.lmn)](example/append.lmn) is the following:

```
ret = append(cons(a, cons(b, nil)), cons(c, nil)).

append_cons @@
R = append(cons(H, T), L) :- R = cons(H, append(T, L)).

append_nil @@
R = append(nil, L) :- R = L.
```

Then, the output will be the following: 

```
0: append(cons(a, cons(b, nil)), cons(c, nil), ret)
----> append_cons
1: cons(a, append(cons(b, nil), cons(c, nil)), ret)
----> append_cons
2: cons(a, cons(b, append(nil, cons(c, nil))), ret)
----> append_nil
3: cons(a, cons(b, cons(c, nil)), ret)
Final state:
cons(a, cons(b, cons(c, nil)), ret)
```


 
