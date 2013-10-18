/*
  Product Inventory Project - Creates classes that can be used to
  implement an immutable inventory system.
  
  The Product class holds a price, id, and quantity on hand.
  
  The Inventory class holds an array of all products on hand,
  as well as performing various operations on the products/inventory
*/


case class Product(price: Double, id: Int, quantity: Int) extends Ordered[Product]{
  override def toString() =
    "ID: " ++ id.toString ++ " quantity: " ++ quantity.toString ++ " $" ++ price.toString
  // The functions below serve adjust the various values of a
  // product by returning a new Product object with the correct value
  def setPrice(new_price: Double): Product = Product(new_price, id, quantity)
  def setID(new_id: Int): Product = Product(price, new_id, quantity)
  def setQuantity(new_quantity: Int) = Product(price, id, new_quantity)
  
  // These methods are used for sales and receiving
  // sell one Product
  def sell(): Product = Product(price, id, quantity - 1)
  // Receive q Products and add them to the current quantity
  def receive(q: Int): Product = Product(price, id, quantity + q)
  
  // Set compare (from Ordered) to compare product IDs for sorting
  def compare(that: Product) = id - that.id
}

class Inventory(ps: Array[Product]){

  // in the primary constructor we make sure that the inventory is sorted
  val products = ps.sortBy(_.id)
  // Holds the current value for the entire inventory
  // Does this lazily in case there are many inventory changes once, or it
  // is not needed for some time
  lazy val value = products.foldLeft(0.0)((x: Double, y: Product) => x + y.price * y.quantity)
  
  // Generic constructor to create an empty inventory
  def this() = this(Array())
  
  override def toString =
    (products map (_.toString ++ "\n")).foldLeft("")(_++_)
  // General maintenance methods for Inventory
  
  /* Adds a product to the inventory - if a product with the
    same ID already exists it throws a RunTimeException, otherwise
    it returns a new Inventory with the product added to the array
  */
  def add(p: Product): Inventory = {
    if(products exists (_.id == p.id))
      throw new RuntimeException ("Product ID already exists in current inventory")
    else
      new Inventory(p +: products)
  }
  
  // Returns a new inventory with Product p removed
  def remove(p: Product): Inventory =
    new Inventory( products filter (_ != p))
  // Returns a new inventory with Product with id removed
  def remove(id: Int): Inventory =
    new Inventory(products filter (_.id != id))
}