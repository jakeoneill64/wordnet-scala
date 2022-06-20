import java.io.{BufferedReader, InputStreamReader}
import java.net.URL
import scala.jdk.javaapi.CollectionConverters

class WordNet(synsets: String, hypernyms: String){

  private val synsetsByName = CollectionConverters.asScala(
    new BufferedReader(
      new InputStreamReader(
        new URL(synsets).openStream()
      )
    )
    .lines()
    .iterator()
    ).flatMap { line =>
      val row = line.split(",")
      val nouns = row(1).split("\\s+")
      nouns map { noun => (noun, row(0)) }
    }
    .toMap

  private val hypernymsByName = CollectionConverters.asScala(
    new BufferedReader(
      new InputStreamReader(
        new URL(hypernyms).openStream()
      )
    )
      .lines()
      .iterator()
  )
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
    0
  }

  def sap(nounA: String, nounB: String): String = {
    null
  }

}


