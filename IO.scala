object IO {
  def ReadInput : String = {
    var resp = Console.readLine
    resp.toLowerCase
  }

  def GetResp(prompt1 : String, prompt2 : String, allowed : List[String]) : String = {
    println(prompt1 + " ")
    var resp = ReadInput    
    var done = false
    while (!done) {
      if (resp == "") {
	        print(prompt2 + " ")
	        resp = ReadInput
      } else if (allowed.contains(resp)) {
	      done = true
      } else {
	      println(prompt2 + " ")
	      resp = ReadInput
      }
    }
    resp
  }
}