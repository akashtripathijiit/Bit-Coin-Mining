import akka.actor.Actor
import akka.actor.Props
import akka.routing.ActorRefRoutee
import akka.routing.Router
import akka.routing.RoundRobinRoutingLogic
import akka.routing.RoundRobinRouter

case class workRange(start : Char)

class Master extends Actor {

  var numberOfWorkers: Int = 65
	var elementsFound : Int = 0
	var numberOfZeroes : Int = 1
	var startChar : Char = '!'
	val workers = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(numberOfWorkers)), name = "workers")
	//var workers = context.actorOf(Props(classOf[Worker],numberOfZeroes), "workers")
	for(i <- 0 until numberOfWorkers )
	  workers ! start(numberOfZeroes)
  
  def receive =
  {
    case "exit" =>
	  {	
	    println("band karo dukkan")
	    numberOfWorkers = numberOfWorkers - 1
    	if(numberOfWorkers == 0)
    	{
    		println("Master : Shutting down")
    		context.stop(self)
    	}
	  }
	  case "Give Me Work" =>{ sender ! workRange(startChar)
	    startChar = ((startChar.toInt) + 1).toChar
	  }
	  case found(foundElement) =>
	  {	  //println("Master : found " + foundElement)
	  	  elementsFound = elementsFound + 1
	  	  println("Master : Elements found = " + elementsFound)
		  if(elementsFound >= 100)
		  {  for(i <- 0 until numberOfWorkers )
			  	workers ! "exit"
		  }  
		  else
	  	  {
			  workers ! "continue"
  		  }
	  	  
	 }
  }
}