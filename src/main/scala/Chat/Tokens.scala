package Chat

object Tokens {
  type Token = Int

  // Terms
  val BONJOUR: Token     = 0
  val JE: Token          = 1
  // Actions
  val ETRE: Token        = 2
  val VOULOIR: Token     = 3
  // Operators
  val ET: Token          = 4
  val OU: Token          = 5
  // Products
  val BIERE: Token       = 6
  val CROISSANT: Token   = 7
  // Unknown word
  val UNKNOWN: Token     = 8
  // Utils
  val PSEUDO: Token      = 9
  val NUM: Token         = 10
  val EOL: Token         = 11
  // State of mind
  val ASSOIFFE : Token   = 12
  val AFFAME : Token     = 13

  val APPELER: Token      = 14
  val CONNAITRE: Token      = 15
  val MOI: Token      = 16
  val SOLDE: Token      = 17
  val MARQUE: Token      = 18
  val COMMANDER: Token = 20
  val COMBIEN: Token = 21
  val COUTER: Token = 22
  val QUEL: Token = 23
  val PRIX: Token = 24
  val USELESS: Token = 25

}
