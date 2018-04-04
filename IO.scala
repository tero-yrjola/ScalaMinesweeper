object IO {
  def ReadInput : String = {
    var resp = Console.readLine
    resp.toLowerCase
  }

  def GetResp(prompt1 : String, prompt2 : String, allowed : List[String], allowed2 : List[String] = List()) : String = {
    println(prompt1)
    var resp = ReadInput    
    var done = false
    while (!done) {
      Console.out.flush
      if (resp == "") {
	        println(prompt2)
	        resp = ReadInput
      } else if (!allowed2.isEmpty){
        if(resp.length == 2){
          if(allowed.contains(resp.charAt(0).toString) && allowed2.contains(resp.charAt(1).toString)){
            done = true
          } else{
            println(prompt2)
            resp = ReadInput
          }
        } else{
          println(prompt2)
          resp = ReadInput
        }
      } else if (allowed.contains(resp)) {
	      done = true
      } else {
	      println(prompt2)
	      resp = ReadInput
      }
    }
    resp
  }

  def PrintBoard(board: List[List[Cell]]){
    print("   ")
    val columns = board(0).size
    if (columns < 9) for (i <- 1 to columns) print(i + "   ")
    else{
      for (i <- 1 to 8) print(i + "   ")
      for (i <- 9 to columns) print (i + "  ")
    }
    println()
    for(i <- 0 to board.size -1){
      print((i+97).asInstanceOf[Char] + " ")
      for(j <- 0 to board(0).size -1){
        print(GetPrintable(board(i)(j)))
        print(" ")
        }
      println()
    }
  }

  def GetPrintable[T](typeOfCell: T) = typeOfCell match {
  case _: Empty    => "[ ]"
  case _: Mine => "[x]"
  case _         => "[?]"
  }
}