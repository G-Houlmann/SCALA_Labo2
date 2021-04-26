package Data

import scala.collection.mutable

object UsersInfo {

  private val DEFAULT_ACCOUNT_VALUE = 30

  // Will contain the name of the currently active user; default value is null.
  private var _activeUser: String = null

  // TODO: step 2 - create an attribute that will contain each user and its current balance.
  private val accounts = mutable.Map[String, Double]()

  def getAccountValue(user: String): Double = accounts(user)

  def getAccountValue(): Double = getAccountValue(_activeUser)

  private def createUser(username: String): Unit = {
    accounts(username) = DEFAULT_ACCOUNT_VALUE
  }

  def logIn(user: String): Unit = {
    if(!accounts.contains(user)){
      createUser(user)
    }
    _activeUser = user
  }

  def isAuthenticate: Boolean = _activeUser != null

  /**
    * Update an account by decreasing its balance.
    * @param user the user whose account will be updated
    * @param amount the amount to decrease
    * @return the new balance
    */
  // TODO: step 2
  def purchase(user: String, amount: Double): Double = {
    accounts(user) -= amount
    accounts(user)
  }

  def purchase(amount: Double): Double = {
    purchase(_activeUser, amount)
  }
}
