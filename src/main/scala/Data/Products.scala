package Data

object Products {
  type Product = Int
  val BEER = 0
  val CROISSANT = 1

  private val DEFAULT_BEER = "boxer"
  private val DEFAULT_CROISSANT = "maison"

  // TODO: step 2 - here your will have an attribute that will contain the products (e.g. "bière"), their types (e.g. "Boxer"), and their prices (e.g. 2.0).
  // TODO: step 2 - You will also have to find a way to store the default type/brand of a product.
  private val products : Map[Product, Map[String, Double]] =
  Map(
    BEER -> Map(
      "farmer" -> 1.0,
      "boxer" -> 1.0,
      "wittekop" -> 2.0,
      "punkipa" -> 3.0,
      "jackhammer" -> 3.0,
      "tenebreuse" -> 4.0
    ),
    CROISSANT -> Map(
      "maison" -> 2.0,
      "cailler" -> 2.0
    )
  )

  def getBeerPrice(brand: String = DEFAULT_BEER): Double = {
    products(BEER)(brand)
  }

  def getCroissantPrice(brand: String = DEFAULT_CROISSANT): Double = {
    products(CROISSANT)(brand)
  }
}
