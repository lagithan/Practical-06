object InventorySystem extends App {

  // Defining the Product case class
  case class Product(id: Int, name: String, quantity: Int, price: Double)

  // Example inventory maps
  val inventory1: Map[Int, Product] = Map(
    101 -> Product(101, "Table", 10, 7000),
    102 -> Product(102, "Chair", 5, 2500),
    103 -> Product(103, "Dressing table", 2,10000)
  )

  val inventory2: Map[Int, Product] = Map(
    101 -> Product(101, "Table", 7,8500),
    104 -> Product(104, "Bearau", 3,15000)
  )

  // Retrieve all product names from inventory1
  def printProductNames(inventory: Map[Int, Product]): Unit = {
    inventory.values.foreach(product => println(product.name))
  }

  // Calculate the total value of all products in inventory1
  def calculateTotalValue(inventory: Map[Int, Product]): Double = {
    inventory.values.map(p => p.quantity * p.price).sum
  }

  // Check if inventory1 is empty
  def isInventoryEmpty(inventory: Map[Int, Product]): Boolean = {
    inventory.isEmpty
  }

  // Merge inventory1 and inventory2, retaining maximum price and updated quantities
  def mergeInventories(inventory1: Map[Int, Product], inventory2: Map[Int, Product]): Map[Int, Product] = {
    var updatedInventory = inventory1

    inventory1.foreach { case (id, product) =>
      if (inventory2.contains(id)) {
        val product2 = inventory2(id)
        val updatedProduct = product.copy(
          quantity = product.quantity + product2.quantity,
          price = math.max(product.price, product2.price)
        )
        updatedInventory = updatedInventory.updated(id, updatedProduct)
      }
    }

    val remainingInventory2 = inventory2.filterNot { case (id, _) =>
      updatedInventory.contains(id)
    }

    updatedInventory ++ remainingInventory2
  }

  // Check if a product with a specific ID exists and print its details
  def checkProductExists(inventory: Map[Int, Product], productId: Int): Unit = {
    inventory.get(productId) match {
      case Some(product) => println(s"Product with ID $productId exists: $product")
      case None => println(s"Product with ID $productId does not exist in Inventory")
    }
  }

  // Testing the functions
  println("Product names in inventory 1 are:")
  printProductNames(inventory1)

  println("Total Value of Inventory 1: Rs " + calculateTotalValue(inventory1))

  println("Is Inventory 1 Empty : " + isInventoryEmpty(inventory1))

  val mergedInventory = mergeInventories(inventory1, inventory2)
  println("Merged Inventory: " + mergedInventory)

  val productIdToCheck = 102
  checkProductExists(inventory1, productIdToCheck)
}
