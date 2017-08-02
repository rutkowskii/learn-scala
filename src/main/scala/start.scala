import scala.io.Source


class Scrobble(val artist: String, val album: String, val title: String){
  override def toString: String = s"[$artist], [$album], [$title]"
}



class ArtistIndex(val name: String, val scrobbles: List[Scrobble] = Nil){
  def append(s : Scrobble): ArtistIndex ={
    new ArtistIndex(name, s :: scrobbles)
  }
}


object LearnScala {
  def main(args: Array[String]) {
    println("we about to start!!")
    val filePath = "C:\\repos\\learn-scala\\src\\main\\scala\\ruciofsky.csv"

    val artistIndices = readFile(filePath)
      .map(makeAnObject)
      .toList.groupBy(s => s.artist)
      .map(pair => new ArtistIndex(pair._1, pair._2))
      .toList

    artistIndices
      .sortBy(a => -(a.scrobbles.length))
      .foreach(a => println(s"NAME = ${a.name} LENGTH = ${a.scrobbles.length}"))


      //.map(x => s"just read: [$x]")
      // .foreach(println)
  }

  def readFile(filePath: String) : Iterator[String] = {
    for (line <- Source.fromFile(filePath).getLines) yield line
  }

  def makeAnObject(input: String) : Scrobble = {
    val splitProducts = input.split(',')
    if(splitProducts.length > 4){
      println("warn for: " + input)
    }
    new Scrobble(splitProducts(0), splitProducts(1), splitProducts(2))
  }


}