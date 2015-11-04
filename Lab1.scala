package security_lab

object Lab1 {
  
  def main(args: Array[String]): Unit = {
    val in = "ВОТПРИМЕРШИФРАВЕРТИКАЛЬНОЙПЕРЕСТАНОВКИ";
    val key = Array(5, 	1, 	4, 	7, 	2, 	6, 	3 );
    println("key: \n" +key.mkString)
    val rect = buildRect(in,key,0,in.length())
    rect foreach (println (_))
    println ("encoded: " + encode(in,key))
    println ("_"*50)
    println ("decoded: " + decode(encode(in,key),key))
    
  }
  
  def decode(in: String, key: Array[Int]):String = {
    val kl = key.length
    val grouped = in.grouped(in.length()/kl).toList
    val rect = (key map (k => grouped(k-1))).toList
    val res =  0 to (in.length()/kl-1) map (i => (rect map (s => s(i))).mkString)
    res.mkString
  }

  def encode(in: String, key: Array[Int]):String = {
     val rect = buildRect(in,key,0,in.length())
     val res = key map (i => rect map (string => string(i-1)))
     key map (k => res(k-1).mkString) mkString
  }
  
  def buildRect(in: String, key: Array[Int],pos:Int,slen:Int):List[String] = {
    val kl = key.length
    if  (kl < slen){
      List(in.subSequence(pos, pos+kl).toString()) ::: buildRect(in,key,pos + kl,slen-kl)
    }else {
      val noise = kl - slen
      List(in.subSequence(pos, pos+slen).toString()++"-"*noise)
    }
    
  }

}