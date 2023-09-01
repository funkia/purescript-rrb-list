"use strict";

import * as L from "list"

export const _eq = L.equalsWith;

export const _append = L.concat;

export const _mempty = L.empty;

export const _map = L.map;

export const _apply = L.ap;

export const _pure = L.of;

export const _foldl = L.foldl;

export const _foldr = L.foldr;

export const _bind = L.chain;

export const _traverse = () => {
  class Cont {
    constructor(fn) {
      this.fn = fn;
    }
  }

  const cons = (x) => (xs) => L.prepend(x, xs);

  return function (apply, map, pure, f) {
    var buildFrom = function (x, ys) {
      return apply(map(cons)(f(x)))(ys);
    };

    var go = function (acc, currentLen, xs) {
      if (currentLen === 0) {
        return acc;
      } else {
        var last = L.nth(currentLen - 1, xs);
        return new Cont(function () {
          return go(buildFrom(last, acc), currentLen - 1, xs);
        });
      }
    };

    return function _traverse(l) {
      var result = go(pure(L.empty()), l.length, l);
      while (result instanceof Cont) {
        result = result.fn();
      }

      return result;
    };
  };
}

export const _show = (f, xs) => "(RRBList [" + L.join(", ", L.map(f, xs)) + "])";

export const _unfoldr1 = (isNothing, fromJust, fst, snd, f, b) => {
  var result = L.empty();
  var value = b;
  while (true) { // eslint-disable-line no-constant-condition
    var pair = f(value);
    result = L.append(fst(pair), result);
    var next = snd(pair);
    if (isNothing(next)) {
      return result;
    } else {
      value = fromJust(next);
    }
  }
};

export const _unfoldr = (isNothing, fromJust, fst, snd, f, b) => {
  var result = L.empty();
  var value = b;
  while (true) { // eslint-disable-line no-constant-condition
    var maybe = f(value);
    if (isNothing(maybe)) return result;
    var tuple = fromJust(maybe);
    result = L.append(fst(tuple), result);
    value = snd(tuple);
  }
};
