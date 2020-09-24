package chapter3

object Exercise28 extends App {
  println(Tree.map(Leaf(3))(_ + 1))
  println(Tree.map(Branch(Leaf(3), Branch(Leaf(5), Leaf(2))))(_ + 1))
  println(
    Tree.map(
      Branch(
        Leaf(3),
        Branch(Branch(Branch(Leaf(5), Leaf(2)), Leaf(2)), Leaf(2))
      )
    )(_ + 1)
  )
}
