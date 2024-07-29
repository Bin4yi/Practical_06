import scala.io.StdIn.readLine

case class Product(name: String, quantity: Int, price: Double)

object InventoryManagement {

  def getProductNames(inventory: Map[Int, Product]): List[String] = {
    inventory.values.map(_.name).toList
  }

  def calculateTotalValue(inventory: Map[Int, Product]): Double = {
    inventory.values.map(p => p.quantity * p.price).sum
  }

  def isInventoryEmpty(inventory: Map[Int, Product]): Boolean = {
    inventory.isEmpty
  }

  def mergeInventories(inventory1: Map[Int, Product], inventory2: Map[Int, Product]): Map[Int, Product] = {
    (inventory1.keySet ++ inventory2.keySet).map { id =>
      val product1 = inventory1.get(id)
      val product2 = inventory2.get(id)

      id -> ((product1, product2) match {
        case (Some(p1), Some(p2)) => 
          Product(p1.name, p1.quantity + p2.quantity, p1.price.max(p2.price))
        case (Some(p), None) => p
        case (None, Some(p)) => p
        case _ => throw new Exception("This should never happen")
      })
    }.toMap
  }

//   def getProductDetails(inventory1: Map[Int, Product], id: Int): Option[Product] = {
//     inventory1.get(id)
//   }

    def getProductDetails(inventory1: Map[Int, Product], inventory2: Map[Int, Product], id: Int): Option[(Product, List[String])] = {
        val product1 = inventory1.get(id)
        val product2 = inventory2.get(id)
        
        (product1, product2) match {
        case (Some(p1), Some(p2)) =>
            Some((p1, List("inventory1", "inventory2")))
        case (Some(p), None) =>
            Some((p, List("inventory1")))
        case (None, Some(p)) =>
            Some((p, List("inventory2")))
        case _ => None
        }
    }

    def displayInventory(inventory: Map[Int, Product]): Unit = {
        if (inventory.isEmpty) {
        println("The inventory is empty.")
        } else {
        inventory.foreach { case (id, product) =>
            println(f"ID: $id, Name: ${product.name}, Quantity: ${product.quantity}, Price: $$${product.price}%.2f")
        }
        }
    }

  def main(args: Array[String]): Unit = {
    var inventory1 = Map(
      101 -> Product("Laptop", 10, 999.99),
      102 -> Product("Smartphone", 20, 499.99),
      103 -> Product("Tablet", 15, 299.99)
    )
    
    var inventory2 = Map(
      102 -> Product("Smartphone", 5, 549.99),
      104 -> Product("Headphones", 30, 99.99)
    )

    var continue = true
    while (continue) {
      println("\n--- Inventory Management System ---")
      println("1. Display product names")
      println("2. Calculate total inventory value")
      println("3. Check if inventory is empty")
      println("4. Merge inventories")
      println("5. Check product details")
      println("6. Display all inventories")
      println("7. Exit")
      print("Enter your choice: ")
      
      readLine().trim match {
        case "1" => 
            println("Product names in inventory1:")
            getProductNames(inventory1).foreach(println)
          
        case "2" => 
            val totalValue = calculateTotalValue(inventory1)
            println(f"Total value of inventory1: $$${totalValue}%.2f")
          
        case "3" => 
            println(s"Is inventory1 empty? ${isInventoryEmpty(inventory1)}")
            println(s"Is inventory2 empty? ${isInventoryEmpty(inventory2)}")
          
        case "4" => 
                val mergedInventory = mergeInventories(inventory1, inventory2)
                println("Merged inventory:")
                mergedInventory.foreach { case (id, product) =>
                    println(f"ID: $id, Name: ${product.name}, Quantity: $$${product.quantity}, Price: $$${product.price}%.2f")
                }
                inventory1 = mergedInventory

        // case "5" => 
        //     print("Enter product ID to check: ")
        //     val idToCheck = readLine().toInt
        //     getProductDetails(inventory1, inventory2, idToCheck) match {
        //         case Some(product) => 
        //         println(f"Product found - Name: ${product.name}, Quantity: ${product.quantity}, Price: $$${product.price}%.2f")
        //         case None => 
        //         println(s"No product found with ID $idToCheck")
        // }  

        case "5" => 
            print("Enter product ID to check: ")
            val idToCheck = readLine().toInt
            getProductDetails(inventory1, inventory2, idToCheck) match {
                case Some((product, inventories)) => 
                    val inventoriesList = inventories.mkString(" and ")
                    println(f"Product found in $inventoriesList - Name: ${product.name}, Quantity: ${product.quantity}, Price: $$${product.price}%.2f")
                case None => 
                    println(s"No product found with ID $idToCheck")
        }

        case "6" =>
            println("Details of inventory1:")
            displayInventory(inventory1)
            println("Details of inventory2:")
            displayInventory(inventory2)

        case "7" => 
            continue = false
            println("Exiting...")
          
        case _ =>   
            println("Invalid choice. Please try again.")
      }
    }
  }
  
}
