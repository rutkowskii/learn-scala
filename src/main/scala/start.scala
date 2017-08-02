import scala.io.Source


class Scrobble(val artist: String, val album: String, val title: String){
  override def toString: String = s"[$artist], [$album], [$title]"
}

class AlbumIndex(val name: String, val scrobbles: List[Scrobble] = Nil)

class ArtistIndex(val name: String, val albums: List[AlbumIndex] = Nil){
  def scrobblesSize  = albums.aggregate(a, b => a.)
}


object LearnScala {
  def main(args: Array[String]) {
    println("we about to start!!")
    val filePath = "C:\\repos\\learn-scala\\src\\main\\scala\\ruciofsky.csv"

    val artistIndices = readFile(filePath)
      .map(makeAnObject)
      .toList
      .groupBy(s => s.artist)
      .map(pair => new ArtistIndex(pair._1, buildAlbumIndices(pair)))
      .toList

    artistIndices
      .sortBy(a => -(a.scrobblesSize))
      .foreach(a => printSummary(a))

  }

  def printSummary(a: ArtistIndex) = {
    println(s"NAME = ${a.name} LENGTH = ${a.albums.length}")

    for(album <- a.albums.sortBy(alb => -(alb.scrobbles.length))){
      println(s"\t\t[${album.name}]\t\t[${album.scrobbles.length}]")
    }
  }

  private def buildAlbumIndices(pair: (String, List[Scrobble])) = {
    pair._2.groupBy(s => s.album).map(pair => new AlbumIndex(pair._1, pair._2)).toList
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