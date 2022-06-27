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

  def distance(nounA: String, nounB: String): Int = sap(nounA, nounB).split("-").length - 1

  def sap(nounA: String, nounB: String): String = search(nounA, nounB).map(id => String valueOf id).mkString("-")

  case class Node(id: Int, noun: String, var next: Option[Node]){

    val thisNode: Node = this

    @tailrec
    private def step(current: Node, next: Node): Node = {
      val tmp = next.next
      next.next = Option(current)
      if(tmp.isDefined)
        step(next, tmp.get)
      else
        next
    }


    def join(node: Node): Node = {
      step(this, node)
    }


    def toList: List[Node] = {

      new Iterator[Node]{
        private var current: Option[Node] = Option(thisNode)

        override def next(): Node = {
          val old = current.get
          current = current.get.next
          old
        }

        override def hasNext: Boolean = current.isDefined

      }.toList

    }

  }

  def search(nounA: String, nounB: String): List[Int] = {

    val nounToNodes = (noun: String) => idsByNoun
      .getOrElse(noun, throw IllegalArgumentException())
      .map {id => Node(id, noun, Option.empty)}

    val startNodesA = nounToNodes(nounA)
    val startNodesB = nounToNodes(nounB)

    val queueA = mutable.Queue[Node]()
    val queueB = mutable.Queue[Node]()

    startNodesA.foreach(node => queueA.enqueue(node))
    startNodesB.foreach(node => queueB.enqueue(node))

    bfs_(queueA, queueB, mutable.Map[Int, Node](), 0).toList.map {node => node.id}
  }

  def bfs_(queueA: mutable.Queue[Node], queueB:mutable.Queue[Node], visitedById: mutable.Map[Int, Node], toMove: Int): Node = {

    val activeQueue = {
      val either = if (toMove % 2 == 0) queueA else queueB
      val other = if (toMove % 2 == 1) queueA else queueB
      if(either.nonEmpty) either else other
    }

    val current = activeQueue.dequeue()

    val alreadyVisited = visitedById.get(current.id)

    if(alreadyVisited.isDefined) {

      if(alreadyVisited.get.noun.equals(current.noun))
        bfs_(queueA, queueB, visitedById, toMove+1)
      else
        current.next.get.join(alreadyVisited.get)

    } else{

      hypernymsById(current.id).map{id => Node(id, current.noun, Option(current))}.foreach{node => activeQueue.enqueue(node)}
      visitedById.put(current.id, current)
      bfs_(queueA, queueB, visitedById, toMove + 1)

    }



  }




}


