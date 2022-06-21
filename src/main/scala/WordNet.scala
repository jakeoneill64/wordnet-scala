import java.io.{BufferedReader, InputStreamReader}
import java.net.URL
import scala.jdk.javaapi.CollectionConverters
import scala.collection.mutable

class WordNet(synsets: String, hypernyms: String){

  private def downloadLines(file: String): Iterator[String] =
    CollectionConverters.asScala(new BufferedReader(new InputStreamReader(new URL(file).openStream())).lines().iterator())

  private val synsetsByName = downloadLines(synsets)
    .flatMap { line =>
      val row = line split ","
      val nouns = row(1) split "\\s+"
      nouns map { noun => (noun, row(0)) }
    }
    .toMap

  private val hypernymsByName = downloadLines(hypernyms)
  .map{line =>
      val row = line split ","
      (row(0), CollectionConverters.asScala(java.util.Arrays.stream(row).skip(1).collect(java.util.stream.Collectors.toList)))
  }
  .toMap
 

  def nouns(): Iterable[String] = {
    synsetsByName.keys
  }

  def isNoun(word: String): Boolean = {
    synsetsByName.contains(word)
  }

  def distance(nounA: String, nounB: String): Int = {
    sap(nounA, nounB).split("-").length
  }

  def sap(nounA: String, nounB: String): String = {


  }

  def bfs(idA: Int, idB: Int): Int = {
    val aVisited: mutable.Set[Int] = mutable.Set()
    val bVisited: mutable.Set[Int] = mutable.Set()
    val aQueue: mutable.Queue[Int] = mutable.Queue()
    val bQueue: mutable.Queue[Int] = mutable.Queue()
    while(){
    }
  }

}


