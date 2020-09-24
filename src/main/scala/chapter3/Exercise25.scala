package chapter3

object Exercise25 extends App {
  println(Tree.size(Leaf(3)))
  println(Tree.size(Branch(Leaf(3), Branch(Leaf(5), Leaf(2)))))
}
