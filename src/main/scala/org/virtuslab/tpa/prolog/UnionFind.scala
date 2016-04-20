package org.virtuslab.tpa.prolog

import scala.collection.GenTraversableOnce

private class UnionFind[A] private(rank: Map[A, Int], var parent: Map[A, A]) {
  @inline def apply(x: A) = find(x)

  def find(x: A): A = {
    val xParent = parent(x)
    if (xParent == x) xParent
    else {
      val (f, xRoot) = findAux(parent, xParent)
      parent = if (xRoot != xParent) f.updated(x, xRoot) else f
      xRoot
    }
  }

  private def findAux(h: Map[A, A], x: A): (Map[A, A], A) = {
    val hx = h(x)
    if (hx == x) (h, x)
    else {
      val (f, r) = findAux(h, hx)
      (if (r != hx) f.updated(x, r) else f, r)
    }
  }

  def equiv(x: A, y: A): Boolean = {
    val xRoot = find(x)
    val yRoot = find(y)
    xRoot == yRoot
  }

  def union(x: A, y: A): UnionFind[A] = {
    val xRoot = find(x)
    val yRoot = find(y)
    if (xRoot != yRoot) unionAux(xRoot, yRoot) else this
  }

  private def unionAux(xRoot: A, yRoot: A): UnionFind[A] = {
    val xRank = rank.getOrElse(xRoot, 0)
    val yRank = rank.getOrElse(yRoot, 0)
    if (xRank < yRank) new UnionFind(rank, parent.updated(xRoot, yRoot))
    else if (xRank > yRank) new UnionFind(rank, parent.updated(yRoot, xRoot))
    else new UnionFind(rank.updated(xRoot, xRank + 1), parent.updated(yRoot, xRoot))
  }

  def +(elem: A): UnionFind[A] = if (parent contains elem) this else new UnionFind(rank, parent + (elem -> elem))

  def ++(that: GenTraversableOnce[A]): UnionFind[A] = (this /: that) (_ + _)
}

private object UnionFind {
  def makeSet[A](xs: GenTraversableOnce[A]): UnionFind[A] = {
    val setBuilder = Map.newBuilder[A, A]
    xs foreach (x => setBuilder += (x -> x))
    new UnionFind(rank = Map.empty, parent = setBuilder.result())
  }

  def empty[A] = new UnionFind[A](Map.empty, Map.empty)
}
