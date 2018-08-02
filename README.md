# purescript-rrb-list

[![Latest release](http://img.shields.io/github/release/funkia/purescript-rrb-list.svg)](https://github.com/funkia/purescript-rrb-list/releases)
[![Build status](https://travis-ci.org/funkia/purescript-rrb-list.svg?branch=master)](https://travis-ci.org/funkia/purescript-rrb-list)
[![Documentation](https://img.shields.io/badge/documentation-pursuit-45516b.svg)](http://pursuit.purescript.org/packages/purescript-rrb-list)

A highly efficient immutable list implementing the `Data.Array` API.

## About

purescript-rrb-list is PureScript bindings for the TypeScript library
[List](https://github.com/funkia/list) which is a highly optimized
implementation of the data-structure
[RRB-trees](https://infoscience.epfl.ch/record/169879/files/RMTrees.pdf).

purescript-rrb-list implements the `Data.Array` API and can serve as a
drop-in replacement for `Data.Array`.

## Why use purescript-rrb-list

JavaScript's native arrays are designed for imperative programming.
Due to the nature of arrays they cannot benefit from structural
sharing and any operation that changes an array must copy the entire
array.

RRB-trees, which purescript-rrb-list implements, offers good time
complexity and low constants for a wide range of operations. This
makes it very well suited as a general purporse immutable list.

The table below compares the running-time of key operations.

| Operation  | purescript-rrb-list | purescript-array | purescript-list |
| ---------- | ------------------- | ---------------- | --------------- |
| `cons`     | `O(1)`              | `O(n)`           | `O(1)`          |
| `snoc`     | `O(1)`              | `O(n)`           | `O(n)`          |
| `append`   | `O(log(max(n, m))`  | `O(n + m)`       | `O(n + m)`      |
| `slice`    | `O(log(n))`         | `O(n)`           | `O(n)`          |
| `insertAt` | `O(log(n))`         | `O(n)`           | `O(n)`          |
| `head`     | `O(1)`              | `O(1)`           | `O(1)`          |
| `last`     | `O(1)`              | `O(1)`           | `O(n)`          |

## When not to use purescript-rrb-list

purescript-rrb-list is very fast for all operations which makes it
suitable as a go-to general purpose immutable list. However, this
means that for use cases that only require few operations it is often
possible to find a specialized data-structure that is faster.

For instance, even though purescript-rrb-list has `O(1)` running-time
for both `cons` and `tail` a single-linked list has lower constants
for these two operations. Thus for use cases that only require a stack
a single-linked list is faster.

Use purescript-rrb-list if:

* You need a wide range of operations. In this case RRB-trees cannot be beat.
* If performance isn't a major concern. In this case you will benefit
  from the generality of RRB-trees and get good performance without
  having to think about it.

Do not use purescript-rrb-list if:

* You have more specialized use-case that only require few operations
  and performance is critical. In this case it is likely that you can
  find a specialized data-structure that performs better.

## Installation

First install the TypeScript library from npm.

```
npm install list
```

And then the PureScript bindings from Bower.

```
bower install --save purescript-rrb-list
```

## Benchmarks

The TypeScript library on which purescript-rrb-list is based provides
[an extensive benchmark
suite](https://funkia.github.io/list/benchmarks/) that compares the
implementation to JavaScript arrays and other immutable lists.

purescript-rrb-list has also been inserted into the benchmark suite by
[purescript-sequences](https://github.com/hdgarrood/purescript-sequences/issues/31#issue-308669682).
The results can be seen in [this
comment](https://github.com/hdgarrood/purescript-sequences/issues/31#issue-308669682).

## API Docs

API documentation is [published on
Pursuit](http://pursuit.purescript.org/packages/purescript-rrb-list).