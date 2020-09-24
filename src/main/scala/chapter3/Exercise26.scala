package chapter3

object Exercise26 extends App {
  println(Tree.maximum(Leaf(3)))
  println(Tree.maximum(Branch(Leaf(3), Branch(Leaf(5), Leaf(2)))))
}
