import java.security.MessageDigest
import akka.actor.Actor
import scala.collection.mutable.StringBuilder
import akka.dispatch.Foreach

case class found(element : String)
case class start(numberOfZeroes : Integer, startChar : Char)

object allDone extends Exception {}

class Worker () extends Actor{

  var differenceCharacter : Char = ' '
  var hexToCompare: String = ""
  var startingChar : String = ""
  var allWords = new WordGenerator("abcdefghijklmnopqrstuvwxyz".toCharArray(),1)
  var iterator = allWords.words.iterator
  var lengthOfWords : Int = 1
  var fetchNew : Int = 0
  var wordsDone : Int = 0
  var string : String = ""
  var hashed : String = ""
  var stringsProcessed : Int = 0
  var charSet : String = ""
  
    def createCharSet() = 
    {
    	for(i <- 33 until 127)
    	  {
    	    charSet += i.toChar
    	  }
    }
    
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
	  case start(zeroes, startChar) =>
	  {
	    hexToCompare = createCompareString(zeroes)
	    startingChar = "atripathi" + startChar.toString()
	    createCharSet()
	    sender ! "shouldIContinue"
	  }
	  case continueWork() =>
	  {
	    if(fetchNew == 1)
	    {	allWords = new WordGenerator(charSet.toCharArray(),lengthOfWords)
	    	iterator = allWords.words.iterator
	    	//stringsProcessed = 0
	    	fetchNew = 0
	    }
	    try{
	      //iterator.drop(stringsProcessed * 10000)
	      //println("Processed : " + stringsProcessed)
	      for(i <- 0 to 10000)
	    	
	    	{
	    		if(iterator.hasNext)
	    		{
	    			string = startingChar + (iterator.next())
	    			hashed = cryptographicHash(string)
	    			if(hashed < hexToCompare)
	    			{	println("Worker : " + string + " - " + hashed)
	    				sender ! found(hashed)
	    			}
	    		}
	    		else
	    			throw allDone
	    	}
	      //println("10000 done")
	      //stringsProcessed += 1
	      //println("check : " + stringsProcessed )
	      
	    }
	    catch{
	    	//FOR supressing warnings
	    	case allDone : Throwable =>
	    	{	fetchNew = 1
	    	  	lengthOfWords += 1
	    		println("Words of length " + (lengthOfWords-1) + " finished")
	    		//sender ! "shouldIContinue"
	    	}
	    }
	    sender ! "shouldIContinue"
	  }
//	    word.words.foreach(s => 
//	    {	
//	        //print(s + ", ")
//	    	var string : String = "atripathi" + start + s
//	        var hashed : String = cryptographicHash(string)
//	    	if(hashed < hexToCompare)
//	    	{	println("Worker : " + string + " - " + hashed)
//	    	  sender ! found(hashed)
//	    	}
//	    })
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