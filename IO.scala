object IO {
  def readInput : String = {
    var resp = Console.readLine
    resp.toLowerCase
  }

  def getresp(prompt1 : String, prompt2 : String, allowed : List[String]) : String = {
    println(prompt1 + " ")
    var resp = readInput    
    var done = false
    while (!done) {
      if (resp == "") {
	        print(prompt2 + " ")
	        resp = readInput
      } else if (allowed.contains(resp)) {
	      done = true
      } else {
	      println(prompt2 + " ")
	      resp = readInput
      }
    }
    resp
  }
}