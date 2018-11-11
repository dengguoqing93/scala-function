package chapter1

/**
  * 一个简单的函数式编程的例子
  *
  * @author DGQ
  * @since 2018/11/11
  */
class Cafe {
  def buyCoffee(cc:CreditCard):Coffee={
    val cup = new Coffee
    cc.charge(cup.price)
    cup
  }
}
