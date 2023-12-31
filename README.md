# Whisper

Whisper is a simple toy LISP interpreter written in Haskell.

Below is a simple prime number checker that `whisper` is capable of running.
```lisp
(define (is-prime-helper n x)
  (if (< n (* x x))
      true
      (if (= 0 (% n x))
          false
          (is-prime-helper n (+ 1 x)))))

(define (is-prime n)
  (if (< n 2)
      false
      (is-prime-helper n 2)))

(is-prime 479001598)
(is-prime 479001599)
```

Running the above code sample will print out:
```lisp
false
true
```

## Features
`whisper` supports many features familiar to Lisp derivatives, including:

<details>
    <summary><strong>Boolean Literals</strong></summary>

```lisp
true
false
```

Will print:
```plaintext
true
false
```
🤯🤯🤯
</details>

<details>
    <summary><strong>Integer Literals</strong></summary>

```lisp
42
3
```

Will print:
```plaintext
42
3
```

🤯🤯🤯
</details>

<details>
    <summary><strong>Native functions, such as `+`, `*`</strong></summary>

```lisp
(+ 1 2 3 4 5)
(* 2 4 6)
```

Will print:
```plaintext
15
48
```
</details>

<details>
    <summary><strong>Conditionals</strong></summary>

```lisp
(if true 0 42)
(if (< 3 1) 3 1)
```

Will print:
```lisp
0
1
```
</details>

<details>
    <summary><strong>Define global variables and functions using `define`</strong></summary>

```lisp
(define global 3)

(define (minimum a b)
        (if (< a b) a b))

global
(minimum 7 100)
```

Will print:
```lisp
3
7
```
</details>

<details>
    <summary><strong>Define scope-local variables using `let`</strong></summary>

```lisp
(define x 3)

(let ((x 0)
      (y 20))
     (* x y))
```

Will print:
```lisp
0
```
</details>

<details>
    <summary><strong>Passing functions as values</strong></summary>

```lisp
(define x +)

(x 1 2 3)
((if true + *) 2 4 6)
```

Will print:
```lisp
6
12
```
</details>

<details>
    <summary><strong>Recursion</strong></summary>

```lisp
(define (is-prime-helper n x)
  (if (< n (* x x))
      true
      (if (= 0 (% n x))
          false
          (is-prime-helper n (+ 1 x)))))

(define (is-prime n)
  (if (< n 2)
      false
      (is-prime-helper n 2)))

(is-prime 479001599)
```

Will print:
```lisp
true
```
</details>

## Running the Interpreter

### REPL
You can easily spin up an interactive REPL with [`stack`](https://docs.haskellstack.org/en/stable/).

```bash
$ stack run whisper-exe
>>>
```

### Run a File
You can run a file by passing the filename as an argument.

```bash
$ stack run whisper-exe examples/prime.lisp

false
true
```

### Launch Interactive REPL After Running File
If you pass the `-i` flag before the filename, you can launch a REPL after loading the file.

```bash
$ stack run whisper-exe -i examples/prime.lisp

false
true
>>> (is-prime 7)
true
```

## Implementation Blog
There are 3 blog posts about this project written during development.

* [Part1: Parsing](https://www.pacokwon.org/posts/20230616-haskell-lisp-parsing)
* [Part2: Evaluation](https://www.pacokwon.org/posts/20230618-haskell-lisp-eval)
* [Part3: Extensions](https://www.pacokwon.org/posts/20230702-haskell-lisp-extensions)
