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
    <summary>Boolean Literals</summary>

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
    <summary>Integer Literals</summary>

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
    <summary>Native functions, such as `+`, `*`</summary>

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
    <summary>Conditionals</summary>

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
    <summary>Define global variables and functions using `define`</summary>

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
    <summary>Define scope-local variables using `let`</summary>

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
    <summary>Passing functions as values</summary>

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
    <summary>Recursion</summary>

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
You can simply spin up an interactive REPL with [`stack`](https://docs.haskellstack.org/en/stable/).

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
