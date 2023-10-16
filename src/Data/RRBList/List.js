"use strict";

import * as L from "list"

export const length = L.length;

export const _filter = L.filter;

export const unsafeHead = L.first;

export const unsafeLast = L.last;

export const unsafeTail = L.tail;

export const unsafeInit = L.init;

export const _unsafeIndex = (l, i) => L.nth(i, l);

export const reverse = L.reverse;

export const concat = L.flatten;

export const splitAt = L.splitAt;

export const remove = L.remove;

export const _range = L.range;

export const _replicate = (n, a) => L.repeat(a, n);

export const _cons = L.prepend;

export const _snoc = (l, a) => L.append(a, l)

export const _insertAt = L.insert;

export const _findIndex = L.findIndex;

export const _take = L.take;

export const _drop = L.drop;

export const _takeEnd = L.takeLast;

export const _dropEnd = L.dropLast;

export const _zipWith = L.zipWith;

export const _takeWhile = L.takeWhile;

export const _dropWhile = L.dropWhile;

export const _modifyAt = L.adjust;

export const _updateAt = L.update;

export const _slice = L.slice;

export const _partition = L.partition;

export const _intersperse = L.intersperse;

// The names of these three functions are flipped in purescript-arrays and funkia/list.
export const _sortBy = L.sortWith;
export const _sortWith = L.sortBy;
export const _groupBy = L.groupWith;
