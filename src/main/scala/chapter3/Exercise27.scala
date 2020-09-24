package chapter3

object Exercise27 extends App {
  println(Tree.depth(Leaf(3)))
  println(Tree.depth(Branch(Leaf(3), Branch(Leaf(5), Leaf(2)))))
  println(
    Tree.depth(
      Branch(
        Leaf(3),
        Branch(Branch(Branch(Leaf(5), Leaf(2)), Leaf(2)), Leaf(2))
      )
    )
  )

  println(Tree.depthFromAnswersKey(Leaf(3)))
  println(Tree.depthFromAnswersKey(Branch(Leaf(3), Branch(Leaf(5), Leaf(2)))))
  println(
    Tree.depthFromAnswersKey(
      Branch(
        Leaf(3),
        Branch(Branch(Branch(Leaf(5), Leaf(2)), Leaf(2)), Leaf(2))
      )
    )
  )
}
