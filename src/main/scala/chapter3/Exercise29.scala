package chapter3

object Exercise29 extends App {
  println("size")
  println(Tree.sizeViaFold(Leaf(3)))
  println(Tree.sizeViaFold(Branch(Leaf(3), Branch(Leaf(5), Leaf(2)))))

  println("maximum")
  println(Tree.maximumViaFold(Leaf(3)))
  println(Tree.maximumViaFold(Branch(Leaf(3), Branch(Leaf(5), Leaf(2)))))

  println("depth")
  println(Tree.depthViaFold(Leaf(3)))
  println(Tree.depthViaFold(Branch(Leaf(3), Branch(Leaf(5), Leaf(2)))))
  println(
    Tree.depthViaFold(
      Branch(
        Leaf(3),
        Branch(Branch(Branch(Leaf(5), Leaf(2)), Leaf(2)), Leaf(2))
      )
    )
  )

  println("map")
  println(Tree.mapViaFold(Leaf(3))(_ + 1))
  println(Tree.mapViaFold(Branch(Leaf(3), Branch(Leaf(5), Leaf(2))))(_ + 1))
  println(
    Tree.mapViaFold(
      Branch(
        Leaf(3),
        Branch(Branch(Branch(Leaf(5), Leaf(2)), Leaf(2)), Leaf(2))
      )
    )(_ + 1)
  )
}
