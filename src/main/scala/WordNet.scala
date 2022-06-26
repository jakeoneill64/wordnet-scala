import java.io.{BufferedReader, InputStreamReader}
import java.net.URL
import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.jdk.javaapi.CollectionConverters
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class WordNet(synsets: String, hypernyms: String){

  private def downloadLines(file: String): Iterator[String] =
    CollectionConverters.asScala(new BufferedReader(new InputStreamReader(new URL(file).openStream())).lines().iterator())

  val idsByNoun: Map[String, List[Int]] = downloadLines(synsets)
    .flatMap { line =>
      val elements = line split ","
      val nouns = elements(1).split("\\s+")
      nouns.map {noun => (elements(0), noun)}
    }
    .toList
    .groupBy{ nounAndId => nounAndId._2}
    .map{(noun: String, nounsAndId: List[(String, String)]) =>
      (noun, nounsAndId.map {nounAndId => Integer parseInt nounAndId._1})
    }


  val hypernymsById: Map[Int, List[Int]] = downloadLines(hypernyms)
  .map{line =>
      val row = line split ","
      (Integer parseInt row(0), row.drop(1).map{hypernym => Integer parseInt hypernym}.toList)
  }
  .toMap

  def nouns(): Iterable[String] = {
    idsByNoun.keys
  }

  def isNoun(word: String): Boolean = {
    idsByNoun contains word
  }

  def distance(nounA: String, nounB: String): Int = sap(nounA, nounB).split("-").length

  def sap(nounA: String, nounB: String): String = {
    val idA = idsByNoun.getOrElse(nounA, throw IllegalArgumentException())
    val idB = idsByNoun.getOrElse(nounB, throw IllegalArgumentException())
    bfs(idA, idB).map(id => String valueOf id).mkString("-")
  }


  def bfs(idA: List[Int], idB: List[Int]): List[Int] = {



  }




}


