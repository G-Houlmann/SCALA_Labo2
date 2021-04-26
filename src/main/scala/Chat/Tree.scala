package Chat

import Data.Products._
import Data.UsersInfo._

// TODO - step 3
object Tree {

  /**
    * This sealed trait represents a node of the tree and contains methods to compute it and write its output text in console.
    */
  sealed trait ExprTree {
    /**
      * Compute the price of the current node, then returns it. If the node is not a computational node, the method
      * returns 0.0.
      * For example if we had a "+" node, we would add the values of its two children, then return the result.
      * @return the result of the computation
      */
    def computePrice: Double = this match {
      case Command(exp) => exp.computePrice
      case Price(exp) => exp.computePrice
      case DefaultItem(product, quantity) => quantity * getPrice(product)
      case Item(product, brand, quantity) => quantity * getPrice(product, brand)
      case And(lexp, rexp) => lexp.computePrice + rexp.computePrice
      case Or(lexp, rexp) => Math.min(lexp.computePrice, rexp.computePrice)
      case _ => 0
    }

    private def authenticationMiddleware: Boolean = this match {
      case Thirsty()
        | Hungry()
        | Identification(_)
        | Price(_)
        => true
      case _ => isAuthenticate
    }

    private def replyCommand(command: Command): String = {
      def loop(node: ExprTree): String = node match {
        case DefaultItem(product, quantity) => s"${quantity} ${asString(product)}"
        case Item(product, brand, quantity) =>s"${quantity} ${asString(product, brand)}"
        case And(lexp, rexp) => s"${loop(lexp)} et ${loop(rexp)}"
        case Or(lexp, rexp) => if (lexp.computePrice < rexp.computePrice) loop(lexp) else loop(rexp)
      }

      val price = command.computePrice
      val balance = purchase(price)
      s"Voici donc ${loop(command.items)} ! Cela coûte CHF ${price} et votre nouveau solde est de CHF ${balance}."
    }

    /**
      * Return the output text of the current node, in order to write it in console.
      * @return the output text of the current node
      */
    def reply: String = {
      if (!this.authenticationMiddleware) {
        return "Veuillez d’abord vous identifier."
      }

      this match {
        case Thirsty() => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
        case Hungry() => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"
        case Identification(name) =>logIn(name); s"Bonjour ${name} !"
        case Balance() => s"Le montant actuel de votre solde est de CHF ${getAccountValue()}"
        case Price(_) => s"Cela coûte CHF ${this.computePrice}"
        case c: Command => replyCommand(c)
        case _ => "Je n'ai rien compris à votre charabia, essayez en klingon la prochaine fois"
      }
    }
  }

  /**
    * Declarations of the nodes' types.
    */
  // Example cases
  case class Thirsty() extends ExprTree
  case class Hungry() extends ExprTree

  // Operators
  case class And(lexp: ExprTree, rexp: ExprTree) extends ExprTree
  case class Or(lexp: ExprTree, rexp: ExprTree) extends ExprTree

  // Action cases
  case class Identification(name: String) extends ExprTree
  case class Command(items: ExprTree) extends ExprTree
  case class Balance() extends ExprTree
  case class Price(items: ExprTree) extends ExprTree

  // Items
  case class DefaultItem(product: Product, quantity: Int) extends ExprTree
  case class Item(product: Product, brand: String, quantity: Int) extends ExprTree
}
