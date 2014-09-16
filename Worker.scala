import java.security.MessageDigest
import akka.actor.Actor
import scala.collection.mutable.StringBuilder

case class found(element : String)
case class start(numberOfZeroes : Integer)



class Worker () extends Actor{

  var hexToCompare: String = ""
  def cryptographicHash(data : String) : String =
  {
	val byteArray = MessageDigest.getInstance("SHA-256").digest(data.getBytes("UTF-8"))
    val hashedString = byteArray.map("%02x".format(_)).mkString
    return hashedString
  }
  
  def createCompareString(input : Int) : String =
  {
	  val str = new StringBuilder
	  for(i <- 1 until 65)
	  {
	    if(i == input)
	      str.append('1')
	    else
	      str.append('0')
	  }
	 return str.toString()
  }
  
  def hex2bytes(hex: String): Array[Byte] =
  {
	hex.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }
 
  def receive =
    {
	  case start(zeroes) =>
	  {
	    hexToCompare = createCompareString(zeroes);
	    //println(cryptographicHash("adobra;kjsdfk11"))
	    //println(hexToCompare)
		//if(cryptographicHash("adobra;kjsdfk11") < hexToCompare)
		//  println("found !")
	    sender ! "Give Me Work"
	  }
	  case workRange(start : Char) =>
	  {
	    val word = new WordGenerator("abcdefghijklmnopqrstuvwxyz".toCharArray(),1)
	    word.words.foreach(s => 
	    {	
	        //print(s + ", ")
	    	var string : String = "atripathi" + start + s
	        var hashed : String = cryptographicHash(string)
	    	if(hashed < hexToCompare)
	    	{	println("Worker : " + string + " - " + hashed)
	    	  sender ! found(hashed)
	    	}
	    })
//		println("Worker : Got - " + start)
//	    for(i:Char <- start until 'A')
//		  {
//	    		var hashed : String = cryptographicHash("atripathi" + i)
//	    		println("Worker : "+ hashed)
//	    		if(hashed < hexToCompare)
//	    			sender ! found(hashed)
//	    	  //if(i == toFind)
//			  //{   println("mil gaya")
//			  //    sender ! found(i)
//			  //}
//	      }
		  sender ! "exit"//"Give Me Work"
	  }
	  case "exit" =>
	  {	sender ! "exit"
	    println("Worker : closing")
	    context.stop(self)
	  }
	}
}

class WordGenerator (alphabet: Array[Char], wordlength: Int) {

  private val MAXWORDS = math.round(math.pow(alphabet.length, wordlength))
  private val RADIX = alphabet.length

  val words = wordStream(0)

  
  private def wordStream(n: Int): Stream[String] = {
    n match {
      case MAXWORDS => Stream.empty
      case _ => val indices = convertToRadix(n)
        (
          for( k <- 0 until wordlength)
            yield alphabet(indices(k))
        ).mkString("") #:: wordStream(n+1)
    }
  }
  
  private def convertToRadix(number: Long): List[Int] = 
    (number match {
      case 0 => Nil
      case _ => (number % RADIX).toInt :: convertToRadix(number / RADIX) 
    } ) padTo(wordlength, 0)
}