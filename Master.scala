import akka.actor.Actor
import akka.actor.Props
import akka.routing.ActorRefRoutee
import akka.routing.Router
import akka.routing.RoundRobinRoutingLogic
import akka.routing.RoundRobinRouter
import com.sun.org.apache.xpath.internal.operations.Bool

case class continueWork()

class Master extends Actor {

	// SET THESE VARIABLES USING ARG LIST OR MANUALLY AS REQUIRED
	var numberOfWorkers: Int = 4
	var elementsToFind : Int = 3
	var numberOfZeroes : Int = 5
	
	var elementsFound : Int = 0
	var startChar : Char = '!'
	var CheckIfExitSent : Int = 0
	val workers = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(numberOfWorkers)), name = "workers")
	//var workers = context.actorOf(Props(classOf[Worker],numberOfZeroes), "workers")
	for(i <- 0 until numberOfWorkers )
	{   workers ! start(numberOfZeroes, startChar)
		startChar = ((startChar.toInt) + 1).toChar
	}
  def receive =
  {
    case "exit" =>
	  {	
	    numberOfWorkers = numberOfWorkers - 1
    	if(numberOfWorkers == 0)
    	{
    		println("Master : Shutting down")
    		context.stop(self)
    	}
	  }
	  case "shouldIContinue" => {
	    if(elementsFound <= elementsToFind )
	    	sender ! continueWork()
	    else if(CheckIfExitSent == 0)
	    {  	CheckIfExitSent = 1
	    	for(i <- 0 until numberOfWorkers )
			  	workers ! "exit"
	    }
	  }
	  case found(foundElement) =>
	  {	  //println("Master : found " + foundElement)
	  	  elementsFound = elementsFound + 1
	  	  println("Master : Elements found = " + elementsFound)
		  if(elementsFound >= elementsToFind )
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