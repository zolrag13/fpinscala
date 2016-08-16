package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  //Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6))))

  //          /\
  //         /  \
  //        /\  /\
  //       1 2 /  \
  //          /\  /\
  //         3 4  5 6
  //
  // Leafs: 6, Branches: 5, Total: 11

  // EXECUTION:
  //
  // size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6)))))
  // [1 + size(Branch(Leaf(1), Leaf(2))) + size(Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6))))]
  // 1 + [1 + size(Left(1)) + size(Left(2))] + [1 + size(Branch(Left(3), Left(4)) + size(Branch(Left(5), Left(6)))]
  // 1 + 3 + 1 + [1 + size(Left(3)) + size(Left(4))] + [1 + size(Left(5)) +  size(Left(6))]
  // 1 + 3 + 1 + 3 + 3
  // 11

  //Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(6)))

  //          /\
  //         /  \
  //        /\  /\
  //       1 2 /  6
  //          /\
  //         3 4
  //
  // Leafs: 5, Branches: 4, Total: 9

  // EXECUTION:
  //
  // size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(6))))
  // [1 + size(Branch(Leaf(1), Leaf(2))) + size(Branch(Branch(Leaf(3), Leaf(4)), Leaf(6)))]
  // 1 + [1 + size(Left(1)) + size(Left(2))] + [1 + size(Branch(Left(3), Left(4)) + size(Left(6))]
  // 1 + 3 + 1 + [1 + size(Left(3)) + size(Left(4))] + 1
  // 1 + 3 + 1 + 3 + 1
  // 9

  def max(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => max(l) max max(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(g: A => B)(f: (B, B) => B): B = t match {
    case Leaf(v) => g(v)
    case Branch(l, r) => f(fold(l)(g)(f), fold(r)(g)(f))
  }

  def size_2[A](t: Tree[A]): Int =
    fold(t)(v => 1)((l, r) => 1 + l + r)

  def depth_2[A](t: Tree[A]): Int =
    fold(t)(v => 0)((l, r) => 1 + (l max r))

  def map_2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf[B](f(v)): Tree[B])((l: Tree[B], r: Tree[B]) => Branch(l, r))

  def max_2(t: Tree[Int]): Int =
    fold(t)(v => v)((l, r) => l max r)
}
