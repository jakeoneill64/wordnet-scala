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

  val idsBySynset: Map[String, Int] = downloadLines(synsets)
    .map { line =>
      val row = line split ","
      (row(1), Integer parseInt row(0))
    }
    .toMap

  val hypernymsById: Map[Int, List[Int]] = downloadLines(hypernyms)
  .map{line =>
      val row = line split ","
      (Integer parseInt row(0), row.drop(1).map{hypernym => Integer parseInt hypernym}.toList)
  }
  .toMap

  def nouns(): Iterable[String] = {
    idsBySynset.keys
  }

  def isNoun(word: String): Boolean = {
    idsBySynset.contains(word)
  }

  def distance(nounA: String, nounB: String): Int = sap(nounA, nounB).split("-").length

  def sap(nounA: String, nounB: String): String = {
    val idA = idsBySynset.getOrElse(nounA, throw IllegalArgumentException())
    val idB = idsBySynset.getOrElse(nounB, throw IllegalArgumentException())
    bfs(idA, idB).map(id => String valueOf id).mkString("-")
  }

  private case class Node(queueNum: Int, id: Int, var next: Option[Node]){
    override def equals(obj: Any): Boolean = obj match {case p: Node => p.id.equals(id) case p:Any => false}
    override def hashCode(): Int = id.hashCode()
    def walkNode(current: Node, buffer: ListBuffer[Int]): ListBuffer[Int] = {
      buffer.addOne(current.id)
      current.next match {
        case Some(node) => walkNode(node, buffer)
        case None => buffer
      }
    }
    def toList: List[Int] = {
      walkNode(this, ListBuffer()).toList
    }

    def joinList(head: Option[Node]): Node = {
      head match {
        case Some(node) =>
          val next = node.next
          node.next = Option(this)
          joinList(next)
        case None => this
      }
    }

  }

  def bfs(idA: Int, idB: Int): List[Int] = {

    val nodeA = Node(0, idA, Option.empty)
    val nodeB = Node(1, idB, Option.empty)

    val nodesById: mutable.Map[Int, Node] = mutable.Map[Int, Node](nodeA.id -> nodeA, nodeB.id -> nodeB)

    val queueA: mutable.Queue[Node] = mutable.Queue[Node](nodeA)
    val queueB: mutable.Queue[Node] = mutable.Queue[Node](nodeB)

    def visit(toMove: Int): List[Int] = {
      val activeQueue = if(toMove % 2 == 0) queueA else queueB
      if(activeQueue.isEmpty) return visit(toMove +1)
      val current = activeQueue.dequeue()

      //check if we have found a path
      val meetingNode = hypernymsById(current.id)
        .map(id => Node(toMove % 2, id, Option(current)))
        .find(node => nodesById.get(node.id) match {
          case Some(visited) => visited.queueNum.equals(toMove % 2)
          case None => false
        })

      if(meetingNode.isDefined) {

          val currentVisited = nodesById.get(current.id)
          current.joinList(meetingNode).toList

      }
      else{

        //add the hypernyms to be visited
        hypernymsById(current.id)
          .map(id => Node(toMove % 2, id, Option(current)))
          .filter(node => nodesById.get(node.id) match {
            case Some(visited) => false // must have been visited b
            case None => true
          })
          .foreach(node => {
            nodesById.put(node.id, node)
            activeQueue.enqueue(node)
          }
          )

        visit(toMove + 1)
      }

    }
    visit(0)

  }




}


