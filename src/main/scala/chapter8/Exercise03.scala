package chapter8

object Exercise03 extends App {
  trait Prop {
    def check: Boolean
    def &&(p: Prop): Prop = new Prop {
      def check = this.check && p.check
    }
  }
}
