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

  val synsetsByName = downloadLines(synsets)
    .flatMap { line =>
      val row = line split ","
      val nouns = row(1) split "\\s+"
      nouns map { noun => (noun, row(0)) }
    }
    .toMap

  val hypernymsById: Map[Int, List[Int]] = downloadLines(hypernyms)
  .map{line =>
      val row = line split ","
      (Integer parseInt row(0), row.drop(1).map{hypernym => Integer parseInt hypernym}.toList)
  }
  .toMap

  def nouns(): Iterable[String] = {
    synsetsByName.keys
  }

  def isNoun(word: String): Boolean = {
    synsetsByName.contains(word)
  }

  def distance(nounA: String, nounB: String): Int = sap(nounA, nounB).split("-").length

  def sap(nounA: String, nounB: String): String = {
  }

  private case class Node(queueNum: Int, id: Int, var previous: Option[Node]) extends Iterable[Int]{
    override def equals(obj: Any): Boolean = obj match {case p: Node => p.id.equals(id) case p:Any => false}
    override def hashCode(): Int = id.hashCode()
    override def toList: List[Int] = {
      def walkNode(current: Node, buffer: ListBuffer[Int]): ListBuffer[Int] = {
        buffer.addOne(current.id)
        current.previous match {
          case Some(node) => walkNode(node, buffer)
          case None => buffer
        }
      }
      walkNode(this, ListBuffer()).toList
    }
  }

  def bfs(idA: Int, idB: Int): List[Int] = {

    val nodesById: mutable.Map[Int, Node] = mutable.Map[Int, Node]()

    val queueA: mutable.Queue[Node] = mutable.Queue[Node](Node(0, idA, Option.empty))
    val queueB: mutable.Queue[Node] = mutable.Queue[Node](Node(1, idB, Option.empty))

    def visit(toMove: Int): List[Int] = {
      val activeQueue = if(toMove % 2 == 0) queueA else queueB
      val current = activeQueue.dequeue()


      hypernymsById(current.id)
        .map(id => Node(toMove % 2, id, Option(current)))
        .find()
        )

      if(nodesById.contains(toMove)) {

          val previous = current.previous.get
          val currentVisited = nodesById(current.id)

          @tailrec
          def iterateAndSwap(current: Node, next: Option[Node]): Node = {
            next match {
              case Some(node) =>
                val next = node.previous
                node.previous = Option(current)
                iterateAndSwap(node, next)
              case None => current
            }
          }

          val result = iterateAndSwap(previous, Option(currentVisited)).toList
      }else{
        hypernymsById(current.id)
          .map(id => Node(toMove % 2, id, Option(current)))
          .filter(node => nodesById.contains(cu))
        activeQueue.enqueue()
      }


    }




  }




}


